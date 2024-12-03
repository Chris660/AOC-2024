pragma Ada_2022;

with Ada.Characters.Handling;
with Ada.Text_IO;

procedure Main is
    use Ada.Text_IO;

    Parse_Error : exception;

    Total : Natural := 0;

    function Parse_Int return Natural is
        package Natural_IO is new Integer_IO (Num => Natural);
        Result : Natural;
    begin
        Natural_IO.Get (Result);
        if Result not in 1 .. 999 then
            raise Parse_Error;
        end if;
        return Result;
    end Parse_Int;

    procedure Expect(C : in Character) is
        Input : Character;
    begin
        Get (Input);
        if Input /= C then
            raise Parse_Error;
        end if;
    end Expect;
        
    procedure Expect(S : in String) is
    begin
        for C of S loop
            Expect(C);
        end loop;
    end Expect;

begin
    while not End_Of_File loop
        declare
            A : Natural;
            B : Natural;
        begin
            Expect("mul(");
            A := Parse_Int;
            Expect(',');
            B := Parse_Int;
            Expect(')');

            Total := @ + A * B;
        exception
            when others =>
                null;
        end;
    end loop;
    New_Line;
    Put_Line("Total: " & Total'Image);
end Main;
