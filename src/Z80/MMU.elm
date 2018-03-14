module Z80.MMU
    exposing
        ( MMU
        , init
        , loadRom
        , readByte
        , readWord
        , writeByte
        , writeWord
        )

import Bitwise
import Byte exposing (Byte)
import Memory exposing (Memory)
import Word exposing (Word)


{-| Z80 memory mapping unit
-}
type alias MMU =
    { ram : Memory
    , rom : Memory
    , sprites : Memory
    , eram : Memory
    , vram : Memory
    , zpage : Memory
    , bankOffset : Int
    , extRamOffset : Int
    }


init : MMU
init =
    { ram = Memory.init ramSize
    , rom = Memory.init romSize
    , eram = Memory.init ramSize
    , vram = Memory.init ramSize
    , zpage = Memory.init 128
    , sprites = Memory.init 160
    , bankOffset = 0
    , extRamOffset = 0
    }


{-| Loads a list of bytes into ROM.
-}
loadRom : List Byte -> MMU -> MMU
loadRom rom mmu =
    { mmu | rom = Memory.initFromCodes rom romSize }


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
                    ( addr + (mmu.bankOffset * romSize)
                    , mmu.rom
                    )

                VRAM addr ->
                    ( addr, mmu.vram )

                ExternalRAM addr ->
                    ( addr + (mmu.extRamOffset * ramSize)
                    , mmu.eram
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
            mmu

        ROMBank1 ->
            Debug.crash "Bank switching not implemented!"

        VRAM addr ->
            { mmu | vram = Memory.writeByte addr val mmu.vram }

        ExternalRAM addr ->
            { mmu | eram = Memory.writeByte addr val mmu.eram }

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
    else if addr <= 0xFFFF then
        ZeroPageRam <| Bitwise.and 0x7F addr
    else if addr == 0xFFFF then
        InterruptEnable
    else
        Invalid


{-| Mask used to access an 8k address for ram.
-}
ramMask : Int -> Int
ramMask =
    Bitwise.and 0x1FFF


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
