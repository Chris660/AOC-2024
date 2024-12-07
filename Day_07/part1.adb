pragma Ada_2022;

with Ada.Characters.Handling;
with Ada.Text_IO;

procedure Part1 is
    use Ada.Text_IO;

    -- Input won't fit in 32-bits
    type Number is range 0 .. 2 ** 64 - 1;
    type Number_Array is array (Positive range <>) of Number;

    -- Input processing
    function Get_Parameters (Line   : in String;
                             Target : out Number) return Number_Array is
        package Large_Int_IO is new Ada.Text_IO.Integer_IO (Num => Number);
        use Large_Int_IO;

        -- Index of the colon character.
        function Sep_Index (Line : in String) return Positive is
        begin
            for I in Line'Range loop
                if Line (I) = ':' then
                    return I;
                end if;
            end loop;
            pragma Assert (False);
        end Sep_Index;

        -- Determine the number of values in the provided line.
        function Count_Values (Line : in String) return Positive is
            In_Number : Boolean := False; -- Are we currently in a number?
            Count     : Natural := 0;
        begin
            for C of Line loop
                case C is
                    when '0' .. '9' =>
                        if not In_Number then
                            In_Number := True;
                            Count := @ + 1;
                        end if;
                    when others =>
                        In_Number := False;
                end case;
            end loop;
            return Count;
        end Count_Values;

        function Get_Values (Line : in String) return Number_Array is
            Count : constant Positive := Count_Values (Line);
            Pos   : Positive := Line'First;
        begin
            return Result : Number_Array (1 .. Count) do
                for I in Result'Range loop
                    Get (From => Line (Pos .. Line'Last),
                         Item => Result(I),
                         Last => Pos);
                    Pos := @ + 1; -- move past the last digit
                end loop;
            end return;
        end Get_Values;

    Colon : constant Positive := Sep_Index(Line);
    Tmp   : Positive; -- XXX

    begin
        Get (Line (Line'First .. Colon - 1), Target, Tmp);
        return Get_Values (Line (Colon + 1 .. Line'Last));
    end Get_Parameters;


    -- The task at hand
    type Inequality is (Higher, Equal, Lower);
    
    function Compare (A, B : in Number) return Inequality is
        (if A < B then Lower elsif A > B then Higher else Equal);

    -- Non recursive wrapper
    function Find_Solution (Target   : in Number;
                            Operands : in Number_Array) return Inequality is

        -- Recursive case
        function Solve (Target   : in Number;
                        Operands : in Number_Array;
                        Acc      : in Number) return Inequality is
            Temp : Number;
        begin
            if Operands'Length = 0 then
                return Compare (Acc, Target);
            end if;

            -- If multiplying the operand by the accumulator exceeds the
            -- target we know this is the wrong approach.
            Temp := Acc * Operands (Operands'First);

            if Compare (Temp, Target) /= Higher then
                if Solve (Target, Operands (Operands'First + 1 .. Operands'Last), Temp) = Equal then
                    return Equal;
                end if;
            end if;

            Temp := Acc + Operands (Operands'First);
            return Solve (Target, Operands (Operands'First + 1 .. Operands'Last), Temp);
        end Solve;

    begin
        return Solve (Target,
                      Operands (Operands'First + 1 .. Operands'Last),
                      Operands (Operands'First));
    end Find_Solution;

    Sum : Number := 0;
begin
    while not End_Of_File loop
        declare
            Target : Number;
            Values : Number_Array := Get_Parameters (Get_Line, Target);
        begin
            if Find_Solution (Target, Values) = Equal then
                Sum := @ + Target;
            end if;
        end;
    end loop;

    Put_Line ("Part 1 sum: " & Sum'Image);
end;

