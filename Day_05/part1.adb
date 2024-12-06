pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;

procedure Part1 is
    type Page_Number is new Positive range 1 .. 99;

    -- Set to store pages which must follow the page in question
    package Page_Sets is new
        Ada.Containers.Ordered_Sets
            (Element_Type => Page_Number);
    use Page_Sets;

    -- Hash map associating a page number with its Successor_Set
    package Page_Order_Maps is new
        Ada.Containers.Ordered_Maps
            (Key_Type     => Page_Number,
             Element_Type => Page_Sets.Set);

    -- Determine whether page updates are correctly ordered.
    type Update_Array is array (Positive range <>) of Page_Number;
    function Is_Ordered (Update      : in Update_Array;
                         Page_Orders : Page_Order_Maps.Map) return Boolean is
        use Page_Order_Maps;
        L, R : Page_Number;
        C    : Page_Order_Maps.Cursor;
    begin
        for I in Update'First .. Update'Last - 1 loop
            L := Update (I);
            R := Update (I + 1);
            C := Page_Orders.Find (L);
            if C = Page_Order_Maps.No_Element or else not Page_Orders (C).Contains (R) then
                return False;
            end if;
        end loop;
        return True;
    end Is_Ordered;

    package Page_Number_IO is new Ada.Text_IO.Integer_IO (Num => Page_Number);

    function Parse_Order (Line : String) return Update_Array is
        Result : Update_Array (1 .. Line'Length / 3 + 1);
        Pos    : Positive := 1;
    begin
        for I in Result'Range loop
            Page_Number_IO.Get(Line (Pos .. Line'Last), Result (I), Pos);
            Pos := @ + 2;
        end loop;
        return result;
    end Parse_Order;

    Pages       : Page_Sets.Set;
    Page_Orders : Page_Order_Maps.Map;
    Part1_Total : Natural := 0;
begin
    -- Parse page order rules
    while not End_Of_File loop
        declare
            Line : String := Get_Line;
            A, B : Page_Number;
            C    : Page_Order_Maps.Cursor;
            Inserted : Boolean;
        begin
            -- Blank line section separator.
            exit when Line'Length = 0;

            pragma assert (Line'Length = 5);
            A := Page_Number'Value (Line (1 .. 2));
            B := Page_Number'Value (Line (4 .. 5));
            Page_Orders.Insert (A, C, Inserted);
            Page_Orders (C).Include (B);
            Pages.Include (A);
            Pages.Include (B);
        end;
    end loop;

    -- Parse updates
    while not End_Of_File loop
        declare
            Line    : String := Get_Line;
            Update  : Update_Array := Parse_Order (Line);
            Cleaned : Update_Array := (for I of Update when Pages.Contains(I) => I);
            Mid     : Page_Number;
        begin
            if Is_Ordered (Cleaned, Page_Orders) then
                Mid := Update (Update'First + Update'Length / 2);
                Part1_Total := @ + Natural (Mid);
            end if;
        end;
    end loop;

    Put_Line ("Part1 total: " & Part1_Total'Image);
end Part1;

