pragma Ada_2022;

with Ada.Characters.Handling;
with Ada.Text_IO;

procedure Part2 is
    use Ada.Text_IO;

    -- Parser state machine for string literals.
    type Parse_Literal (Size : Natural) is
        record
            Pos     : Positive := 1;
            Literal : String (1 .. Size);
        end record;

    function Add (P : in out Parse_Literal; C : in Character) return Boolean is
    begin
        if P.Literal (P.Pos) = C then
            if P.Pos < P.Size then
                P.Pos := @ + 1;
            else
                P.Pos := 1;
                return True;
            end if;
        else
            P.Pos := 1;
        end if;
        return False;
    end Add;

    procedure Reset (P : in out Parse_Literal) is
    begin
        P.Pos := P.Literal'First;
    end Reset;


    -- Parser state machine for mul expressions.
    type Mul_State is (M, U, L, Open, A, B);
    type Parse_Multiply is record
        A     : Natural   := 0;
        B     : Natural   := 0;
        NPos  : Positive  := 1;
        State : Mul_State := M;
    end record;

    procedure Reset (Parser : in out Parse_Multiply) is
    begin
        Parser.A     := 0;
        Parser.B     := 0;
        Parser.NPos  := 1;
        Parser.State := M;
    end Reset;

    function Add (Parser : in out Parse_Multiply; C : in Character) return Boolean is
        procedure Advance_If (Parser : in out Parse_Multiply; Test : in Boolean; Next_State : in Mul_State) is
        begin
            if Test then
                Parser.State := Next_State;
            else
                Reset (Parser);
            end if;
        end Advance_If;

        use Ada.Characters.Handling;

        Digit_Value : constant array (Character range '0' .. '9') of Natural := (0, 1, 2, 3, 4, 5, 6, 7, 8, 9);

    begin
        -- Put_Line ("Parser.State: " & Parser.State'Image);
        case Parser.State is
            when M    => Advance_If (Parser, C = 'm', U);
            when U    => Advance_If (Parser, C = 'u', L);
            when L    => Advance_If (Parser, C = 'l', Open);
            when Open => Advance_If (Parser, C = '(', A);

            when A =>
                if Is_Digit (C) then
                    if Parser.NPos <= 3 then
                        Parser.A := @ * 10 + Digit_Value (C);
                        Parser.NPos := @ + 1;
                    else
                        Reset (Parser);
                    end if;
                elsif Parser.NPos > 1 and C = ',' then
                    Parser.NPos  := 1;
                    Parser.State := B;
                else
                    Reset (Parser);
                end if;

            when B =>
                if Is_Digit (C) then
                    if Parser.NPos <= 3 then
                        Parser.B := @ * 10 + Digit_Value (C);
                        Parser.NPos := @ + 1;
                    else
                        Reset (Parser);
                    end if;
                elsif Parser.NPos > 1 and C = ')' then
                    Parser.State := M;
                    Parser.NPos  := 1;
                    return True;
                else
                    Reset (Parser);
                end if;
        end case;
        return False;
    end Add;

    function Next return Character is
        C : Character;
    begin
        Get (C);
        return C;
    end Next;

    -- Parser state
    Do_Machine   : Parse_Literal := (Size => 4, Literal => "do()", others => <>);
    Dont_Machine : Parse_Literal := (Size => 7, Literal => "don't()", others => <>);
    Mul_Machine  : Parse_Multiply;
    Enabled      : Boolean := True;
    C            : Character;
    Total        : Natural := 0;

begin
    while not End_Of_File loop
        C := Next;
        if Enabled then
            if Add (Dont_Machine, C) then
                -- Put_Line ("Disabled");
                Reset (Do_Machine);
                Enabled := False;
            elsif Add (Mul_Machine, C) then
                Total := @ + Mul_Machine.A * Mul_Machine.B;
                Reset (Mul_Machine);
            end if;
        else
            if Add (Do_Machine,  C) then
                -- Put_Line ("Enabled");
                Reset (Mul_Machine);
                Reset (Dont_Machine);
                Enabled := True;
            end if;
        end if;
    end loop;
    New_Line;
    Put_Line("Total: " & Total'Image);
end Part2;
