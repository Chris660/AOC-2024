pragma Ada_2022;

with Ada.Characters.Handling;
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Main is
    use Ada.Text_IO;

    type Report is array (Natural range <>) of Integer;

    -- Determine the number of levels in the provided line.
    function Count_Levels (Line : in String) return Natural is
        use Ada.Characters.Handling;
        In_Level : Boolean := False; -- Are we currently in a number?
        Count : Natural := 0;
    begin
        for C of Line loop
            if not In_Level and then Is_Digit (C) then
                In_Level := True;
                Count := @ + 1;
            else
                In_Level := False;
            end if;
        end loop;
        return Count;
    end Count_Levels;

    function Get return Report is
        use Ada.Integer_Text_IO;

        Line       : constant String  := Get_Line;
        Num_Levels : constant Natural := Count_Levels (Line);
        Pos        : Positive         := Line'First;
    begin
        return Result : Report (1 .. Num_Levels) do
            for I in Result'Range loop
                Ada.Integer_Text_IO.Get (
                    From => Line (Pos .. Line'Last),
                    Item => Result(I),
                    Last => Pos);
                Pos := @ + 1; -- move past the last digit
            end loop;
        end return;
    end Get;

    function Is_Safe (Levels : in Report) return Boolean is
        Diff      : Integer;
        Last_Diff : Integer := 0;
    begin
        for I in Levels'First .. Levels'Last - 1 loop
            Diff := Levels(I + 1) - Levels(I);

            if abs(Diff) < 1 or else abs(Diff) > 3 then
                return False;
            end if;

            if Last_Diff /= 0 then
                if Last_Diff * Diff < 0 then
                    -- Differing signs
                    return False;
                end if;
            end if;
            Last_Diff := Diff;
        end loop;

        return True;
    end Is_Safe;

    Safe_Count : Natural := 0;

begin
    -- Part 1 - count safe levels
    while not End_Of_File loop
        declare
            Levels : constant Report := Get;
        begin
            if Is_Safe (Levels) then
                Safe_Count := @ + 1;
                -- Put (Levels'Image);
            end if;
        end;
    end loop;
    New_Line;
    Put_Line("Safe Count : " & Safe_Count'Image);
end Main;
