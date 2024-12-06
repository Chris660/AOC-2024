pragma Ada_2022;

with Ada.Strings;
with Ada.Text_IO;

procedure Part2 is
    use Ada.Text_IO;

    type Word_Search is array (Positive range <>, Positive range <>) of Character;

    function Read_Input return Word_Search is
        -- This was an experiment to see whether can return a definite array
        -- based on a dynamically determined size.
        procedure Add_Line (Data : in out Word_Search; Row : Positive; S : String) is
        begin
            for Index in S'Range loop
                Data (Row, Index) := S (Index);
            end loop;
        end Add_Line;

        function Read_Rest (First_Line : in String) return Word_Search is
            Dim    : constant Positive := First_Line'Length;
            Result : Word_Search (1 .. Dim, 1 .. Dim);
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

    function Is_Xmas (Data : Word_Search; X, Y : Positive) return Boolean is
        (Data (Y, X) = 'A' and then
         ((Data (Y - 1, X - 1) = 'M' and then Data (Y + 1, X + 1) = 'S') or else
          (Data (Y - 1, X - 1) = 'S' and then Data (Y + 1, X + 1) = 'M')) and then
         ((Data (Y - 1, X + 1) = 'M' and then Data (Y + 1, X - 1) = 'S') or else
          (Data (Y - 1, X + 1) = 'S' and then Data (Y + 1, X - 1) = 'M')));

    Puzzle : constant Word_Search := Read_Input;
    Total  : Natural := 0;
begin
    for Y in Puzzle'First(1) + 1 .. Puzzle'Last(1) - 1 loop
        for X in Puzzle'First(2) + 1 .. Puzzle'Last(2) - 1 loop
            if Is_Xmas (Puzzle, Y, X) then
                Total := @ + 1;
                Put_Line (X'Image & "," & Y'Image);
            end if;
        end loop;
    end loop;

    Put_Line("Xmas count: " & Total'Image);
end Part2;
