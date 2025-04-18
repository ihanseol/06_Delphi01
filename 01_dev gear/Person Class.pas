// Person.pas
unit PersonUnit;

interface

type
  TPerson = class(TObject)
  private
    FFirstName: string;
    FLastName: string;
    FAge: Integer;
    procedure SetFirstName(const Value: string);
    procedure SetLastName(const Value: string);
    procedure SetAge(Value: Integer);
  protected
    // Protected members for inherited classes
    function GetNameParts: TArray<string>; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    // Properties
    property FirstName: string read FFirstName write SetFirstName;
    property LastName: string read FLastName write SetLastName;
    property Age: Integer read FAge write SetAge;

    // Methods
    function GetFullName: string;
    procedure ValidateAge(Value: Integer);
  end;

implementation

constructor TPerson.Create;
begin
  inherited Create;
  FFirstName := '';
  FLastName := '';
  FAge := 0;
end;

destructor TPerson.Destroy;
begin
  // Any cleanup goes here
  inherited Destroy;
end;

procedure TPerson.SetFirstName(const Value: string);
begin
  FFirstName := Trim(Value);
end;

procedure TPerson.SetLastName(const Value: string);
begin
  FLastName := Trim(Value);
end;

procedure TPerson.SetAge(Value: Integer);
begin
  ValidateAge(Value);
  FAge := Value;
end;

function TPerson.GetNameParts: TArray<string>;
begin
  Result := TArray<string>.Create(FFirstName, FLastName);
end;

function TPerson.GetFullName: string;
begin
  Result := Format('%s %s', [FirstName, LastName]);
end;

procedure TPerson.ValidateAge(Value: Integer);
begin
  if (Value < 0) or (Value > 150) then
    raise EInvalidArgument.Create('Invalid age value');
end;

end.



// Usage example
var
  Person: TPerson;
begin
  Person := TPerson.Create;
  try
    Person.FirstName := 'John';
    Person.LastName := 'Smith';
    Person.Age := 25;
    
    ShowMessage(Person.GetFullName); // Shows "John Smith"
  finally
    Person.Free;
  end;


  