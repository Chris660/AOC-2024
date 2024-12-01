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

    procedure Read_Input (A, B : out Location_Array) is
        package Input is new Ada.Text_IO.Integer_IO (Num => Location_ID);
    begin
        for I in A'Range loop
            Input.Get (A (I));
            Input.Get (B (I));
        end loop;
    exception
        when Ada.IO_Exceptions.End_Error =>
            null;
    end Read_Input;

    procedure Sort is
        new Ada.Containers.Generic_Constrained_Array_Sort (
            Location_Index, Location_ID, Location_Array);

    type Distance is new Natural;

    function Distance_Between(A, B : in Location_ID) return Distance is
    begin
        return Distance(abs(B - A));
    end Distance_Between;

    function Distance_Between(A, B : in Location_Array) return Distance is
    begin
        return Result : Distance := 0 do
            for I in A'Range loop
                Result := @ + Distance_Between (A(I), B(I));
            end loop;
        end return;
    end Distance_Between;

    function Similarity_Between_Sorted_Lists(
        Left, Right : in Location_Array
    ) return Natural is
        Result      : Natural := 0;
        Left_Count  : Natural;
        Right_Count : Natural;
        Left_Item   : Location_ID;
        Left_Idx    : Positive := Positive(Left'First);
        Right_Idx   : Positive := Positive(Right'First);
    begin
        while Left_Idx <= Positive(Left'Last) loop
            Left_Item  := Left (Location_Index(Left_Idx));
            Left_Count := 1;

            -- Count consecutive elements in Left
            while Left_Idx < Positive(Left'Last) and then Left (Location_Index(Left_Idx + 1)) = Left_Item loop
                Left_Idx   := @ + 1;
                Left_Count := @ + 1;
            end loop;

            -- Count occurrences of Left_Item in Right
            Right_Count := 0;
            while Right_Idx < Positive(Right'Last) and then Right (Location_Index(Right_Idx)) <= Left_Item loop
                if Right (Location_Index(Right_Idx)) = Left_Item then
                    Right_Count := @ + 1;
                end if;
                Right_Idx := @ + 1;
            end loop;

            -- Accumulate the result
            Result := @ + Natural(Left_Item) * Left_Count * Right_Count;
            Left_Idx := @ + 1;
        end loop;
        return result;
    end Similarity_Between_Sorted_Lists;

    List_A         : Location_Array;
    List_B         : Location_Array;
    Total_Distance : Distance;
    Similarity     : Natural;

    use Ada.Text_IO;

begin
    -- Part 1 - total distance
    Read_Input (List_A, List_B);
    Sort (List_A);
    Sort (List_B);
    Total_Distance := Distance_Between (List_A, List_B);
    Put_Line("Distance   : " & Total_Distance'Image);

    -- Part 2
    Similarity := Similarity_Between_Sorted_Lists (List_A, List_B);
    Put_Line("Similarity : " & Similarity'Image);
end Main;
