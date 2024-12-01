pragma Ada_2022;

with Ada.Containers.Vectors;
with Ada.Text_IO;

procedure Main is
    type Location_ID is new Positive;

    package ID_Vectors is new
        Ada.Containers.Vectors (Index_Type   => Natural,
                                Element_Type => Location_ID);

    package ID_Vectors_Sorting is new ID_Vectors.Generic_Sorting;

    procedure Read_Input (A, B : out ID_Vectors.Vector) is
        use Ada.Text_IO;
        package Input is new Integer_IO (Num => Location_ID);

        Value: Location_ID;
    begin
        while not End_Of_File loop
            Input.Get (Value);
            A.Append(Value);
            Input.Get (Value);
            B.Append(Value);
        end loop;
    end Read_Input;

    -- Distance between Location_IDs
    type Distance is new Natural;

    function Distance_Between(A, B : in Location_ID) return Distance is
    begin
        return Distance(abs(B - A));
    end Distance_Between;

    function Distance_Between(A, B : in ID_Vectors.Vector) return Distance is
    begin
        return Result : Distance := 0 do
            for I in A.First_Index .. A.Last_Index loop
                Result := @ + Distance_Between (A(I), B(I));
            end loop;
        end return;
    end Distance_Between;

    -- Part 2 - calculate a "similarity score" between two Location ID vectors
    function Similarity_Between_Sorted_Lists(
        Left, Right : in ID_Vectors.Vector
    ) return Natural is
        Result      : Natural := 0;
        Left_Count  : Natural;
        Right_Count : Natural;
        Left_Item   : Location_ID;
        Left_Idx    : ID_Vectors.Extended_Index := Left.First_Index;
        Right_Idx   : ID_Vectors.Extended_Index := Right.First_Index;
    begin
        while Left_Idx <= Left.Last_Index loop
            Left_Item  := Left (Left_Idx);
            Left_Count := 1;

            -- Count consecutive elements in Left
            while Left_Idx < Left.Last_Index and then Left (Left_Idx + 1) = Left_Item loop
                Left_Idx   := @ + 1;
                Left_Count := @ + 1;
            end loop;

            -- Count occurrences of Left_Item in Right
            Right_Count := 0;
            while Right_Idx < Right.Last_Index and then Right (Right_Idx) <= Left_Item loop
                if Right (Right_Idx) = Left_Item then
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

    List_A, List_B : ID_Vectors.Vector;
    Total_Distance : Distance;
    Similarity     : Natural;

begin
    -- Part 1 - total distance
    Read_Input (List_A, List_B);
    ID_Vectors_Sorting.Sort (List_A);
    ID_Vectors_Sorting.Sort (List_B);
    Total_Distance := Distance_Between (List_A, List_B);
    Ada.Text_IO.Put_Line("Distance   : " & Total_Distance'Image);

    -- Part 2
    Similarity := Similarity_Between_Sorted_Lists (List_A, List_B);
    Ada.Text_IO.Put_Line("Similarity : " & Similarity'Image);
end Main;
