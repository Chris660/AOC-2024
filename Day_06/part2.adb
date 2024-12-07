pragma Ada_2022;

with Ada.Containers;
with Ada.Containers.Ordered_Sets;
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

    -- A record to store the guard's position
    type Map_Coord is record
        X : Natural;
        Y : Natural;
    end record;

    function "<" (Left, Right : in Map_Coord) return Boolean is
    begin
        if Left.X = Right.X then
            return Left.Y < Right.Y;
        else
            return Left.X < Right.X;
        end if;
    end;

    -- A set of steps, used to determine where the guard has patrolled.
    package Location_Sets is new Ada.Containers.Ordered_Sets
        (Element_Type => Map_Coord);
    use Location_Sets;

    -- A record to store the guards position and direction.
    type Position is record
        Pos : Map_Coord;
        Dir : Direction;
    end record;

    function "<" (Left, Right : in Position) return Boolean is
    begin
        if Left.Pos = Right.Pos then
            return Left.Dir < Right.Dir;
        else
            return Left.Pos < Right.Pos;
        end if;
    end;

    -- A set of steps, used to determine where the guard has patrolled.
    package Position_Sets is new Ada.Containers.Ordered_Sets
        (Element_Type => Position);
    use Position_Sets;

    procedure Find_Guard (Map   : in Puzzle;
                          Guard : out Position) is
    begin
        for Y in Map'Range(2) loop
            for X in Map'Range(1) loop
                if Map (Y, X) = '^' then
                    Guard.Pos.X := X;
                    Guard.Pos.Y := Y;
                    Guard.Dir := Up;
                end if;
            end loop;
        end loop;
        pragma Assert (false);
    end Find_Guard;


    Loop_Detected : exception;

    procedure Patrol (Map   : in Puzzle;
                      Start : in Position;
                      Path  : out Position_Sets.Set) is
        Guard : Position := Start;
    begin
        while Guard.Pos.X in Map'Range(2) and then Guard.Pos.Y in Map'Range(1) loop
            -- Record the guard's position and direction
            if Map (Guard.Pos.Y, Guard.Pos.X) = '.' then
                begin
                    if Path.Contains (Guard) then
                        raise Loop_Detected;
                    end if;
                    Path.Insert (Guard);
                end;
            end if;

            declare
                Next_X : Natural := Guard.Pos.X + Offset_X (Guard.Dir);
                Next_Y : Natural := Guard.Pos.Y + Offset_Y (Guard.Dir);
            begin
                -- Turn to avoid obstacles, checking bounds.
                while Next_X in Map'Range(2) and then
                      Next_Y in Map'Range(1) and then 
                      Map (Next_Y, Next_X) = '#' loop
                    Guard.Dir := Turn(Guard.Dir);
                    Next_X := Guard.Pos.X + Offset_X (Guard.Dir);
                    Next_Y := Guard.Pos.Y + Offset_Y (Guard.Dir);
                end loop;

                -- Step forward
                Guard.Pos.Y := Next_Y;
                Guard.Pos.X := Next_X;
            end;
        end loop;
    end Patrol;

    Guard_Start      : Position;
    Map              : Puzzle := Read_Input;
    Trail            : Position_Sets.Set;
    Visited          : Location_Sets.Set;
    Loop_Found_Count : Natural := 0;

begin
    -- Locate the starting position
    Find_Guard (Map, Guard_Start);

    -- Replace the guard's symbol in the map with empty floor.
    Map (Guard_Start.Pos.Y, Guard_Start.Pos.X) := '.';

    -- Part 1, walk the map with no additional obstructions.
    -- Determine the set of visited locations (ignoring direction).
    Patrol (Map, Guard_Start, Trail);
    for Step of Trail loop
        Visited.include (Step.Pos);
    end loop;
    Put ("Part 1, visited count:");
    Put_Line (Length(Visited)'Image);

    -- Part 2, try obstructing each tile visited in part 1.
    for Pos of Visited loop
        Map (Pos.Y, Pos.X) := '#';
        Trail.Clear;
        begin
            Patrol (Map, Guard_Start, Trail);
        exception
            when Loop_Detected =>
                Loop_Found_Count := @ + 1;
        end;
        Map (Pos.Y, Pos.X) := '.';
    end loop;

    Put ("Part 2, loop count:");
    Put_Line (Loop_Found_Count'Image);
end Part2;
