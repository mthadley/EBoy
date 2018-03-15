module Z80.MMU
    exposing
        ( MMU
        , Mode(..)
        , ROMType(..)
        , ROMWrite(..)
        , init
        , loadRom
        , ramEnableVal
        , readByte
        , readWord
        , romWrite
        , writeByte
        , writeWord
        )

import Bitwise
import Byte exposing (Byte)
import Carry
import Memory exposing (Memory)
import Word exposing (Word)


{-| Z80 memory mapping unit
-}
type alias MMU =
    { ram : Memory
    , rom : Memory
    , sprites : Memory
    , eram : ERAM
    , vram : Memory
    , zpage : Memory
    , bankOffset : Byte
    , extRamOffset : Byte
    , romType : ROMType
    }


init : MMU
init =
    { ram = Memory.init ramSize
    , rom = Memory.init romSize
    , eram = ERAMDisabled <| Memory.init ramSize
    , vram = Memory.init ramSize
    , zpage = Memory.init 128
    , sprites = Memory.init 160
    , bankOffset = Byte.zero
    , extRamOffset = Byte.zero
    , romType = ROMOnly
    }


{-| Loads a list of bytes into ROM.
-}
loadRom : List Byte -> MMU -> MMU
loadRom codes mmu =
    let
        rom =
            Memory.initFromCodes codes romSize
    in
    { mmu
        | rom = rom
        , romType = readROMType rom
    }


{-| Reads a `Byte` from the MMU. Takes a `Word` which is
the address to read from.
-}
readByte : Word -> MMU -> Byte
readByte word mmu =
    let
        addr =
            Word.toInt word

        ( actualAddr, memory ) =
            case memoryRegion word of
                ROMBank0 ->
                    ( addr, mmu.rom )

                ROMBank1 ->
                    ( addr + (Byte.toInt mmu.bankOffset * romSize)
                    , mmu.rom
                    )

                VRAM addr ->
                    ( addr, mmu.vram )

                ExternalRAM addr ->
                    ( addr + (Byte.toInt mmu.extRamOffset * ramSize)
                    , case mmu.eram of
                        ERAMEnabled eram ->
                            eram

                        ERAMDisabled _ ->
                            disabledRAM
                    )

                WorkRAM addr ->
                    ( addr, mmu.ram )

                WorkRAMShadow addr ->
                    ( addr, mmu.ram )

                Sprites addr ->
                    ( addr, mmu.sprites )

                MMIO ->
                    Debug.crash "MMIO reads not implemented!"

                ZeroPageRam addr ->
                    ( addr, mmu.zpage )

                InterruptEnable ->
                    Debug.crash "InterruptEnable not implemented!"

                Invalid ->
                    Debug.crash <|
                        "Reading from invalid memory location: "
                            ++ toString addr
    in
    Memory.readByte actualAddr memory


{-| Reads a `Word` from the MMU. Takes a `Word` which is
the address to read from.
-}
readWord : Word -> MMU -> Word
readWord addr mmu =
    Word.fromBytes
        (readByte (Word.inc addr) mmu)
        (readByte addr mmu)


{-| Writes a `Byte` to the MMU.
-}
writeByte : Word -> Byte -> MMU -> MMU
writeByte word val mmu =
    case memoryRegion word of
        ROMBank0 ->
            writeROM word val mmu

        ROMBank1 ->
            writeROM word val mmu

        VRAM addr ->
            { mmu | vram = Memory.writeByte addr val mmu.vram }

        ExternalRAM addr ->
            case mmu.eram of
                ERAMDisabled _ ->
                    mmu

                ERAMEnabled eram ->
                    { mmu
                        | eram =
                            ERAMEnabled <|
                                Memory.writeByte addr val eram
                    }

        WorkRAM addr ->
            { mmu | ram = Memory.writeByte addr val mmu.ram }

        WorkRAMShadow addr ->
            { mmu | ram = Memory.writeByte addr val mmu.ram }

        Sprites addr ->
            { mmu | sprites = Memory.writeByte addr val mmu.sprites }

        MMIO ->
            Debug.crash "MMIO writes not implemented yet!"

        ZeroPageRam addr ->
            { mmu | zpage = Memory.writeByte addr val mmu.zpage }

        InterruptEnable ->
            Debug.crash "InterruptEnable writes not implemented yet!"

        Invalid ->
            Debug.crash <| "Invalid memory write location: " ++ toString word


{-| Handles writes to ROM bank 0.
-}
writeROM : Word -> Byte -> MMU -> MMU
writeROM word val mmu =
    let
        addr =
            Word.toInt word
    in
    case romWrite mmu.romType addr val of
        Noop ->
            mmu

        ROMBankSelect mask bank ->
            let
                newBankOffset =
                    mmu.bankOffset
                        |> Byte.and mask
                        |> Byte.or bank
            in
            { mmu | bankOffset = newBankOffset }

        RAMBankSelect bank ->
            { mmu | extRamOffset = bank }

        ModeSwitch mode ->
            { mmu | romType = MBC1 mode }

        RAMEnable enabled ->
            { mmu
                | eram =
                    case ( mmu.eram, enabled ) of
                        ( ERAMEnabled memory, True ) ->
                            ERAMEnabled memory

                        ( ERAMDisabled memory, True ) ->
                            ERAMEnabled memory

                        ( ERAMEnabled memory, False ) ->
                            ERAMDisabled memory

                        ( ERAMDisabled memory, False ) ->
                            ERAMDisabled memory
            }


{-| Writes a `Word` to the MMU.
-}
writeWord : Word -> Word -> MMU -> MMU
writeWord addr val memory =
    let
        ( high, low ) =
            Word.toBytes val
    in
    memory
        |> writeByte addr low
        |> writeByte (Word.inc addr) high


{-| Represents external RAM enabled state.
-}
type ERAM
    = ERAMEnabled Memory
    | ERAMDisabled Memory


{-| Determines the operation type of the ROM.
-}
type ROMType
    = ROMOnly
    | MBC1 Mode
    | MBC2
    | MBC3


{-| Determines the operation mode of the ROM:
16Mbit ROM/8KByte RAM or 4Mbit ROM/32KByte RAM
-}
type Mode
    = RAMBankMode
    | ROMBankMode


{-| Takes a `MMU` and returns the current `ROMType`.
-}
readROMType : Memory -> ROMType
readROMType memory =
    case Byte.toInt <| Memory.readByte romTypeAddr memory of
        0x01 ->
            MBC1 ROMBankMode

        0x02 ->
            MBC1 ROMBankMode

        0x03 ->
            MBC1 ROMBankMode

        0x05 ->
            MBC2

        0x06 ->
            MBC2

        0x0F ->
            MBC3

        0x10 ->
            MBC3

        0x11 ->
            MBC3

        0x12 ->
            MBC3

        0x13 ->
            MBC3

        _ ->
            ROMOnly


{-| Represents the logical regions of the MMU.
-}
type Region
    = ROMBank0
    | ROMBank1
    | VRAM Int
    | ExternalRAM Int
    | WorkRAM Int
    | WorkRAMShadow Int
    | Sprites Int
    | MMIO
    | ZeroPageRam Int
    | Invalid
    | InterruptEnable


{-| Determine the Region that is associated to the
address.
-}
memoryRegion : Word -> Region
memoryRegion word =
    let
        addr =
            Word.toInt word
    in
    if addr <= 0x3FFF then
        ROMBank0
    else if addr <= 0x7FFF then
        ROMBank1
    else if addr <= 0x9FFF then
        VRAM <| ramMask addr
    else if addr <= 0xBFFF then
        ExternalRAM <| ramMask addr
    else if addr <= 0xDFFF then
        WorkRAM <| ramMask addr
    else if addr <= 0xFDFF then
        WorkRAMShadow <| ramMask addr
    else if addr <= 0xFE9F then
        Sprites <| addr - 0xFE00
    else if addr <= 0xFF7F then
        MMIO
    else if addr <= 0xFFFE then
        ZeroPageRam <| Bitwise.and 0x7F addr
    else if addr == 0xFFFF then
        InterruptEnable
    else
        Invalid


{-| Represents the type of ROMWrite operations.
-}
type ROMWrite
    = ROMBankSelect Byte Byte
    | RAMBankSelect Byte
    | RAMEnable Bool
    | ModeSwitch Mode
    | Noop


{-| Represents the regions of ROM Writes.
-}
type ROMRegion
    = RAMEnableRegion -- 0x0000 - 01FFF
    | ROMSelectRegion -- 0x2000 - 0x3FFF
    | RAMSelectRegion -- 0x4000 - 0x5FFF
    | ModeSwitchRegion -- 0x6000 - 0x7FFF


{-| Takes an address and determines the logical
region being addressed.
-}
romRegion : Int -> ROMRegion
romRegion addr =
    if addr <= 0x1FFF then
        RAMEnableRegion
    else if addr <= 0x3FFF then
        ROMSelectRegion
    else if addr <= 0x5FFF then
        RAMSelectRegion
    else
        ModeSwitchRegion


{-| Returns the type of ROM write operation based on the
address, value, and ROM type.
-}
romWrite : ROMType -> Int -> Byte -> ROMWrite
romWrite romType addr val =
    case ( romType, romRegion addr ) of
        ( MBC1 _, ModeSwitchRegion ) ->
            ModeSwitch <|
                if Byte.lsbSet val then
                    RAMBankMode
                else
                    ROMBankMode

        ( MBC1 _, ROMSelectRegion ) ->
            val
                |> Byte.and mbc1LowerROMMask
                |> decIfNotZero
                |> ROMBankSelect (Byte.complement mbc1LowerROMMask)

        ( MBC1 ROMBankMode, RAMSelectRegion ) ->
            val
                |> Byte.shiftLeftBy 5
                |> Carry.value
                |> Byte.and mbc1UpperRomMask
                |> ROMBankSelect (Byte.complement mbc1UpperRomMask)

        ( MBC1 RAMBankMode, RAMSelectRegion ) ->
            val
                |> Byte.and mbc1RAMMask
                |> RAMBankSelect

        ( ROMOnly, _ ) ->
            Noop

        ( _, RAMEnableRegion ) ->
            val
                |> Byte.xor ramEnableVal
                |> Byte.isZero
                |> RAMEnable

        -- TODO: Remove this
        ( _, _ ) ->
            Noop


decIfNotZero : Byte -> Byte
decIfNotZero byte =
    if not <| Byte.isZero byte then
        Byte.dec byte
    else
        byte


mbc1RAMMask : Byte
mbc1RAMMask =
    Byte.fromInt 0x03


{-| MBC1 ROM Bank select mask
-}
mbc1LowerROMMask : Byte
mbc1LowerROMMask =
    Byte.fromInt 0x1F


mbc1UpperRomMask : Byte
mbc1UpperRomMask =
    Byte.fromInt 0x60


{-| The address that holds the cartridge type
-}
romTypeAddr : Int
romTypeAddr =
    0x0147


{-| Mask used to access an 8k address for ram.
-}
ramMask : Int -> Int
ramMask =
    Bitwise.and 0x1FFF


{-| Value when written to ROM that enables external
ram.
-}
ramEnableVal : Byte
ramEnableVal =
    Byte.fromInt 0x0A


{-| Size of work RAM.
-}
ramSize : Int
ramSize =
    8192


{-| Hardcoded ROM size. Should hopefully work for most
ROMS right now
-}
romSize : Int
romSize =
    16384


{-| Represents disabled memory. Reads should return zero.
-}
disabledRAM : Memory
disabledRAM =
    Memory.init 0
