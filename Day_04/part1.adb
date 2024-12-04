pragma Ada_2022;

with Ada.Strings;
with Ada.Text_IO;

procedure Part1 is
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

    type Direction is (N, NE, E, SE, S, SW, W, NW);

    function Count_Words_At (
        Data : in Word_Search;
        Word : in String;
        X, Y : in Positive
    ) return Natural is
        Room_N : constant Boolean := Y > 3;
        Room_E : constant Boolean := X <= Data'Last(2) - 3;
        Room_W : constant Boolean := X > 3;
        Room_S : constant Boolean := Y <= Data'Last(1) - 3;
        Count : Natural := 0;
    begin
        -- Check the root matches
        if Data (Y, X) = Word (Word'First) then
            if Room_N then
                if Data (Y - 1, X) = Word (Word'First + 1) and then
                   Data (Y - 2, X) = Word (Word'First + 2) and then
                   Data (Y - 3, X) = Word (Word'First + 3) then
                    Count := @ + 1;
                end if;
            end if;

            if Room_N and then Room_E then
                if Data (Y - 1, X + 1) = Word (Word'First + 1) and then
                   Data (Y - 2, X + 2) = Word (Word'First + 2) and then
                   Data (Y - 3, X + 3) = Word (Word'First + 3) then
                    Count := @ + 1;
                end if;
            end if;

            if Room_E then
                if Data (Y, X + 1) = Word (Word'First + 1) and then
                   Data (Y, X + 2) = Word (Word'First + 2) and then
                   Data (Y, X + 3) = Word (Word'First + 3) then
                    Count := @ + 1;
                end if;
            end if;

            if Room_E and then Room_S then
                if Data (Y + 1, X + 1) = Word (Word'First + 1) and then
                   Data (Y + 2, X + 2) = Word (Word'First + 2) and then
                   Data (Y + 3, X + 3) = Word (Word'First + 3) then
                    Count := @ + 1;
                end if;
            end if;

            if Room_S then
                if Data (Y + 1, X) = Word (Word'First + 1) and then
                   Data (Y + 2, X) = Word (Word'First + 2) and then
                   Data (Y + 3, X) = Word (Word'First + 3) then
                    Count := @ + 1;
                end if;
            end if;

            if Room_S and Room_W then
                if Data (Y + 1, X - 1) = Word (Word'First + 1) and then
                   Data (Y + 2, X - 2) = Word (Word'First + 2) and then
                   Data (Y + 3, X - 3) = Word (Word'First + 3) then
                    Count := @ + 1;
                end if;
            end if;

            if Room_W then
                if Data (Y, X - 1) = Word (Word'First + 1) and then
                   Data (Y, X - 2) = Word (Word'First + 2) and then
                   Data (Y, X - 3) = Word (Word'First + 3) then
                    Count := @ + 1;
                end if;
            end if;

            if Room_W and then Room_N then
                if Data (Y - 1, X - 1) = Word (Word'First + 1) and then
                   Data (Y - 2, X - 2) = Word (Word'First + 2) and then
                   Data (Y - 3, X - 3) = Word (Word'First + 3) then
                    Count := @ + 1;
                end if;
            end if;
        end if;
            
        return Count;
    end Count_Words_At;

    Puzzle : constant Word_Search := Read_Input;
    Dim_Y  : constant Positive := Puzzle'Length(1);
    Dim_X  : constant Positive := Puzzle'Length(2);
    Total  : Natural := 0;
begin
    Put_Line("Puzzle: " & Dim_X'Image & " x" & Dim_Y'Image);

    for Y in Puzzle'Range(1) loop
        for X in Puzzle'Range(2) loop
            Total := @ + Count_Words_At (Puzzle, "XMAS", X, Y);
        end loop;
    end loop;

    Put_Line("Word count: " & Total'Image);
end Part1;
