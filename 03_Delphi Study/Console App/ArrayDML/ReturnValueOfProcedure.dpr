program ReturnValueOfProcedure;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils;


procedure CalculateSum(a, b: Integer; var result: Integer);
begin
  result := a + b;
end;

procedure GetFullName(firstName, lastName: string; out fullName: string);
begin
  fullName := Trim(firstName) + ' ' + Trim(lastName);
end;

var
  sum: Integer;
  name: string;

begin
  CalculateSum(5, 3, sum);
  Writeln('Sum: ', sum); // Output: Sum: 8

  GetFullName('John ', ' Doe', name);
  Writeln('Full Name: ', name); // Output: Full Name: John Doe

  Readln;
end.





