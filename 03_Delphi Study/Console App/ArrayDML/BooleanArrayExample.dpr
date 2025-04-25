program BooleanArrayExample;

{$APPTYPE CONSOLE}

uses
  System.SysUtils;

const
  AfterBefore: array[Boolean] of string = ('Before', 'After');

var
  Condition: Boolean;

begin
  Condition := False;
  Writeln('When condition is False: ', AfterBefore[Condition]); // Output: When condition is False: Before

  Condition := True;
  Writeln('When condition is True: ', AfterBefore[Condition]);  // Output: When condition is True: After

  Readln;
end.
