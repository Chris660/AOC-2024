pragma Ada_2022;

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Text_IO;

procedure Part1 is
    use Ada.Text_IO;

    subtype Antenna is Character;

    type Coordinate is record
        X : Integer := 0;
        Y : Integer := 0;
    end record;

    package Coordinate_Vectors is new Ada.Containers.Vectors
        (Index_Type   => Positive,
         Element_Type => Coordinate);
    use Coordinate_Vectors;

    function Hash (C : in Character) return Ada.Containers.Hash_Type is
        (Ada.Containers.Hash_Type (Character'Pos(C)));

    package Coordinate_Maps is new Ada.Containers.Hashed_Maps
        (Key_Type        => Antenna,
         Element_Type    => Coordinate_Vectors.Vector,
         Hash            => Hash,
         Equivalent_Keys => "=");

    -- For the map
    type Map is array (Positive range <>, Positive range <>) of Character;

    function Read_Map return Map is
        Input : String   := Get_Line;
        Y     : Positive := 1;

        procedure Add_Line (M : in out Map; Y : in Positive; Line : in String) is
        begin
            for X in Line'Range loop
                M (Y, X) := Line (X);
            end loop;
        end Add_Line;

    begin
        return Result : Map (1 .. Input'Length, 1 .. Input'Length) do
            Add_Line (Result, Y, Input);
            while not End_Of_Line loop
                Y := @ + 1;
                Input := Get_Line;
                for X in Input'Range loop
                    Result (Y, X) := Input (X);
                end loop;
            end loop;
        end return;
    end Read_Map;

    Puzzle   : constant Map := Read_Map;
    type Map_Mask is array (Puzzle'Range(1), Puzzle'Range(2)) of Boolean;
    Has_Node : Map_Mask := (others => (others => false));
    Antennas : Coordinate_Maps.Map;

    procedure Record_Antenna_At (Antenna : Character; X, Y : in Positive) is
        use Coordinate_Maps;
        C : Coordinate_Maps.Cursor := Antennas.Find (Antenna);
        N : Boolean;
    begin
        if C = Coordinate_Maps.No_Element then
            Insert (Antennas, Antenna, C, N);
        end if;
        Antennas (C).Append (Coordinate'(X, Y));
    end Record_Antenna_At;

    use Coordinate_Maps;

    function Antinode_Count (A, B   : in Coordinate;
                             Symbol : in Antenna) return Natural is
        X_Off  : constant Integer := B.X - A.X;
        Y_Off  : constant Integer := B.Y - A.Y;
        Result : Natural := 0;

        procedure Place_Node (X, Y : in Integer) is
        begin
            if X in Puzzle'Range(2) and then Y in Puzzle'Range(1) and then not Has_Node (Y, X) then
                Result := @ + 1;
                Has_Node (Y, X) := True;
            end if;
        end Place_Node;

    begin
        Place_Node (A.X - X_Off, A.Y - Y_Off);
        Place_Node (B.X + X_Off, B.Y + Y_Off);
        return Result;
    end Antinode_Count;

    Part1_Count : Natural := 0;

begin
    -- Record all antenna locations in Antennas (could have done this while reading the input).
    for Y in Puzzle'Range(1) loop
        for X in Puzzle'Range(2) loop
            if Puzzle (Y, X) /= '.' then
                Record_Antenna_At (Puzzle (Y, X), X, Y);
            end if;
        end loop;
    end loop;

    -- Consider all pairs of antennas of the same type.
    for C in Antennas.Iterate loop
        declare
            Ants : Coordinate_Maps.Constant_Reference_Type := Constant_Reference (Antennas, C);
        begin
            for A in First_Index (Ants) .. Last_Index (Ants) - 1 loop
                for B in A + 1 .. Last_Index (Ants) loop
                    Part1_Count := @ + Antinode_Count (Ants (A), Ants (B), Key (C));
                end loop;
            end loop;
        end;
    end loop;

    Put_Line ("Part 1 count: " & Part1_Count'Image);
end Part1;
