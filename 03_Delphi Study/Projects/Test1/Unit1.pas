unit unit1;

interface

// declare type, const, var, procedure, function

uses
  vcl.Dialogs;

type
  TPerson = class(TObject)
    Name: string;
    Age : integer;
    Address : string;

    function GetName:string;
    function GetDog: string;
  end;

  TEmp = class(Tperson)
    office : string;

  end;

  Th = class(TEmp)
    hours : integer;
    rate : integer;
  end;

  TDog = class(TObject)
    Name: string;
    Age : integer;
    Address : string;

    function GetName:string;
    function GetDog: string;
  end;

  Country = array [0 .. 100] of string;

  Person = record
    Name: string;
    age: byte;
    address: string;
  end;




  p_person = ^Person; // pointer type of Person

var

  // memory location : heap
  s: string = '100';
  i: integer;
  t: tdatetime;
  b: boolean;
  Countries: Country;
  a: array of string;
  a2: array of array of string;

  p: p_person;
  p1: ^Person;

  ps: pchar; // long *string in c, pansi char, pwidechar, puncodestr

Procedure Test;
function Add(x, y: integer): integer;
function Devide(x, y : integer) : integer; overload;
function Devide(x, y : real) : real; overload;




// function Add(out x,y :integer) : integer;
// function Add(x,y :integer) : integer;- call by value
// function Add(var x,y :integer) : integer; - call by ref
//function Add(const x,y :integer) : integer;


implementation

// declared upper side, implementation ;
// type, var, const �� �����Ҽ� �ִ�.
// in this area declaration is not used other area , ���⼭ �����ϸ� �ٸ� ���ֿ����� ����Ҽ��� ����
// uses
// vcl.Dialogs;

var
  j: integer;



function TPerson.GetDog: string;
begin

end;

function TPerson.GetName:string;
begin


end;




function Devide(x, y : integer) : integer;
begin
  result := x div y;
end;

function Devide(x, y : real) : real;
begin
  result := x / y;
end;


function Add( x, y: integer): integer;
begin

  // Add := x + y;
  // exit(x+y);  // version, 2010

  Result := x + y;

end;

procedure Test;
begin
  var
    k: integer; // memory location - stack , cannot initialize local variable

  showmessage('test msg are called');

end;

// uses ���� ������ ������ �ȴ�., �ַ� �������� �ʱ�ó��, �������� ����Ʈ��, �޸� �Ҵ�
{ TDog }

function TDog.GetDog: string;
begin

end;

function TDog.GetName: string;
begin

end;

initialization

begin
  Countries[0] := 'korea';
  Countries[1] := 'America';
  Countries[2] := 'Japan';

  setlength(a, 2);
  a[0] := 'a';

  setlength(a2, 2, 2);
  a2[0, 0] := 'a2';
  // �̰��� �����ε�� function �� �ȴ�.

end;

finalization

end.
