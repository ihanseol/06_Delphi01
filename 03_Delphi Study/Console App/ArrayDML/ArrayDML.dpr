program ArrayDML;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Rtti;
// 우리는 지금

type
  TPerson = record
    ID: Integer;
    Name: string;
    Age: Integer;
  end;

var
  People: array of TPerson;

procedure InsertPerson(ID: Integer; Name: string; Age: Integer);
var
  NewPerson: TPerson;
begin
  NewPerson.ID := ID;
  NewPerson.Name := Name;
  NewPerson.Age := Age;
  SetLength(People, Length(People) + 1);
  People[High(People)] := NewPerson;
  Writeln('Inserted: ID=', ID, ', Name=', Name, ', Age=', Age);
end;

procedure UpdatePerson(ID: Integer; NewName: string = ''; NewAge: Integer = -1);
var
  i: Integer;
begin
  for i := Low(People) to High(People) do
  begin
    if People[i].ID = ID then
    begin
      if NewName <> '' then
        People[i].Name := NewName;
      if NewAge <> -1 then
        People[i].Age := NewAge;
      Writeln('Updated: ID=', ID, ', Name=', People[i].Name, ', Age=', People[i].Age);
      Exit;
    end;
  end;
  Writeln('Person with ID=', ID, ' not found for update.');
end;

procedure DeletePerson(ID: Integer);
var
  i, j: Integer;
begin
  for i := Low(People) to High(People) do
  begin
    if People[i].ID = ID then
    begin
      // Create a new array one element smaller
      SetLength(People, Length(People) - 1);
      // Shift subsequent elements to fill the gap
      for j := i to High(People) - 1 do
        People[j] := People[j + 1];
      Writeln('Deleted: ID=', ID);
      Exit;
    end;
  end;
  Writeln('Person with ID=', ID, ' not found for deletion.');
end;

procedure SelectAllPeople;
var
  Person: TPerson;
begin
  Writeln('--- All People ---');
  for Person in People do
    Writeln('ID=', Person.ID, ', Name=', Person.Name, ', Age=', Person.Age);
  Writeln('------------------');
end;

function SelectPersonByID(ID: Integer): TPerson;
var
  Person: TPerson;
begin
  Result.ID := -1; // Indicate not found
  for Person in People do
  begin
    if Person.ID = ID then
    begin
      Result := Person;
      Exit;
    end;
  end;
end;

var
  FoundPerson: TPerson;

begin
  // "Insert" operations
  InsertPerson(1, 'Alice', 30);
  InsertPerson(2, 'Bob', 25);
  InsertPerson(3, 'Charlie', 35);

  // "Select" all
  SelectAllPeople;

  // "Update" operation
  UpdatePerson(2, 'Robert', 26);

  // "Select" all after update
  SelectAllPeople;

  // "Select" by ID
  FoundPerson := SelectPersonByID(1);
  if FoundPerson.ID <> -1 then
    Writeln('Found by ID 1: Name=', FoundPerson.Name, ', Age=', FoundPerson.Age);

  FoundPerson := SelectPersonByID(4);
  if FoundPerson.ID <> -1 then
    Writeln('Found by ID 4: Name=', FoundPerson.Name, ', Age=', FoundPerson.Age)
  else
    Writeln('Person with ID 4 not found.');

  // "Delete" operation
  DeletePerson(2);

  // "Select" all after delete
  SelectAllPeople;

  Readln;
end.
