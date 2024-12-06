pragma Ada_2022;

with Ada.Text_IO;

procedure Part1 is
    use Ada.Text_IO;

    type Puzzle is array (Positive range <>, Positive range <>) of Character;

    function Read_Input return Puzzle is
        procedure Add_Line (Data : in out Puzzle; Row : Positive; S : String) is
        begin
            for Index in S'Range loop
                Data (Row, Index) := S (Index);
            end loop;
        end Add_Line;

        function Read_Rest (First_Line : in String) return Puzzle is
            Dim    : constant Positive := First_Line'Length;
            Result : Puzzle (1 .. Dim, 1 .. Dim);
        begin
            Add_Line (Result, 1, First_Line);
            for Row in 2 .. Dim loop
                Add_Line (Result, Row, Get_Line);
            end loop;
            return Result;
        end Read_Rest;

    begin
        return Read_Rest (Get_Line);
    end Read_Input;

    type Direction is (Up, Down, Left, Right);
    Offset_X : constant array (Direction) of Integer range -1 .. 1 := (
        Left => -1, Right => 1, others => 0);
    Offset_Y : constant array (Direction) of Integer range -1 .. 1 := (
        Up => -1, Down => 1, others => 0);
    Turn : constant array (Direction) of Direction := (
        Up => Right, Right => Down, Down => Left, Left => Up);

    procedure Find_Guard (M : Puzzle; Guard_X, Guard_Y : out Positive) is
    begin
        for Y in M'Range(2) loop
            for X in M'Range(1) loop
                if M (Y, X) = '^' then
                    Guard_X := X;
                    Guard_Y := Y;
                end if;
            end loop;
        end loop;
        pragma Assert (false);
    end Find_Guard;

    Guard_Facing     : Direction := Up;
    Guard_X, Guard_Y : Natural; -- needs to be able to step out of the map
    Map              : Puzzle := Read_Input;
    Step_Count       : Natural := 1; -- starting position
begin
    -- Locate the starting position
    Find_Guard (Map, Guard_X, Guard_Y);
    while Guard_X in Map'Range(2) and then Guard_Y in Map'Range(1) loop
        -- Mark (and count) the current position
        if Map (Guard_Y, Guard_X) = '.' then
            Step_Count := @ + 1;
        end if;
        Map (Guard_Y, Guard_X) := 'X';

        declare
            Next_X : Natural := Guard_X + Offset_X (Guard_Facing);
            Next_Y : Natural := Guard_Y + Offset_Y (Guard_Facing);
        begin
            -- Turn to avoid obstacles, checking bounds.
            while Next_X in Map'Range(2) and then
                  Next_Y in Map'Range(1) and then 
                  Map (Next_Y, Next_X) = '#' loop
                Guard_Facing := Turn(Guard_Facing);
                Next_X := Guard_X + Offset_X (Guard_Facing);
                Next_Y := Guard_Y + Offset_Y (Guard_Facing);
            end loop;

            -- Step forward
            Guard_Y := Next_Y;
            Guard_X := Next_X;
        end;
    end loop;

    -- Put_Line ("Map: " & Map'Image);
    Put_Line ("Part 1 count: " & Step_Count'Image);
end;
