pragma Ada_2022;

with Ada.Text_IO;

procedure Part2 is
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
    Start_X, Start_Y : Natural;
    Guard_X, Guard_Y : Natural; -- needs to be able to step out of the map
    Map              : Puzzle := Read_Input;

    -- Part 2
    Move_Count        : Natural := 0;
    Obstruct_After    : Natural := 0;
    Obstruction_X     : Natural := 0;
    Obstruction_Y     : Natural := 0;
    Obstruction_Count : Natural := 0;
    Loop_Found_Count  : Natural := 0;

begin
    -- Locate the starting position
    Find_Guard (Map, Start_X, Start_Y);
    loop
        -- Reset start position
        Guard_X       := Start_X;
        Guard_Y       := Start_Y;
        Guard_Facing  := Up;
        Obstruction_X := 0;
        Obstruction_Y := 0;

        while Guard_X in Map'Range(2) and then Guard_Y in Map'Range(1) loop
            declare
                Next_X : Natural := Guard_X + Offset_X (Guard_Facing);
                Next_Y : Natural := Guard_Y + Offset_Y (Guard_Facing);
            begin
                if Obstruct_After = Move_Count and then
                        Next_X in Map'Range(2) and then Next_Y in Map'Range(1) and then
                        Map (Next_Y, Next_X) = '.' then
                    Obstruction_X := Next_X;
                    Obstruction_Y := Next_Y;
                    Guard_Facing := Turn(Guard_Facing);
                    Next_X := Guard_X + Offset_X (Guard_Facing);
                    Next_Y := Guard_Y + Offset_Y (Guard_Facing);

                elsif Next_X = Obstruction_X and then Next_Y = Obstruction_Y then
                    -- we've looped.
                    Loop_Found_Count := @ + 1;
                    Put_Line ("Loop found");
                    exit;
                end if;

                Move_Count := @ + 1;

                if Next_X in Map'Range(2) and then Next_Y in Map'Range(1) then
                    if Map (Next_Y, Next_X) = '#' then
                        Guard_Facing := Turn(Guard_Facing);
                    else
                        -- Step forward
                        Guard_Y := Next_Y;
                        Guard_X := Next_X;
                    end if;
                else
                    exit;
                end if;
                Put_Line (Move_Count'Image & Guard_X'Image & Guard_Y'Image & Guard_Facing'Image & Loop_Found_Count'Image);
            end;
        end loop;

        Obstruct_After := @ + 1; -- Move one more step before adding the obstacle next time
        exit when Move_Count <= Obstruct_After;
    end loop;

    -- Put_Line ("Map: " & Map'Image);
    Put_Line ("Part 2 loops: " & Loop_Found_Count'Image);
end Part2;
