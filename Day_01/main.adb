pragma Ada_2022;
with Ada.Containers.Generic_Constrained_Array_Sort;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Text_IO;

procedure Main is
    Input_Size : constant Positive := 1000;

    type Location_ID is new Positive;
    type Location_Index is range 1 .. Input_Size;
    type Location_Array is array (Location_Index) of Location_ID;

    type Distance is new Positive;

    function Distance_Between(A, B : in Location_ID) return Distance is
    begin
        return Distance(abs(B - A));
    end Distance_Between;

    procedure Read_Input (A, B : out Location_Array) is
        package Input is new Ada.Text_IO.Integer_IO (Num => Location_ID);
    begin
        for I in A'Range loop
            Input.Get (A (I));
            Input.Get (B (I));
        end loop;
    exception
        when E : Ada.Text_IO.Data_Error =>
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));

        when Ada.IO_Exceptions.End_Error =>
            null;
    end Read_Input;

    procedure Sort is
        new Ada.Containers.Generic_Constrained_Array_Sort (
            Location_Index, Location_ID, Location_Array);
    List_A : Location_Array;
    List_B : Location_Array;
    -- List_C : Location_Array;

    use Ada.Text_IO;
begin
    Read_Input (List_A, List_B);

    for I in 1 .. 10 loop
        Ada.Text_IO.Put (" ");
        Ada.Text_IO.Put (List_A (Location_Index(I))'Image);
    end loop;
    Ada.Text_IO.New_Line;

    Sort (List_A);
    Sort (List_B);

    for I in 1 .. 10 loop
        Ada.Text_IO.Put (" ");
        Ada.Text_IO.Put (List_A (Location_Index(I))'Image);
    end loop;
    Ada.Text_IO.New_Line;

    Put_Line(Distance_Between(5, 2)'Image);
    Put_Line(Distance_Between(2, 5)'Image);
end Main;
