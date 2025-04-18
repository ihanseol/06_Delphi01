{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2024 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Printer;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes, System.Generics.Collections, System.IOUtils, System.SysUtils, System.Types, System.UIConsts,
  System.UITypes, FMX.Dialogs, FMX.Graphics, FMX.MediaLibrary, FMX.Types;

type

{ TPrinterDevice }

  EPrinter = class(Exception);
  EPrinterSettingsError = class(EPrinter);

  TPrinterDevice = class
  private
    FDevice: string;
    FDriver: string;
    FPort: string;

    function GetActiveDPI: TPoint; inline;
    function GetDPI(Index: Integer): TPoint;
    function GetDPICount: Integer; inline;
    procedure SetActiveDPIIndex(const Value: Integer);
  protected
    FDPIList: TList<TPoint>; // list of DPI resolutions
    FActiveDPIIndex: Integer;

    procedure DPIChangeError;

    function GetTitle: string; virtual;

    // refreshes the list of pixel-per-inch (DPI) resolutions
    procedure RefreshDPIList; virtual; abstract;
    // notified when the user changed the ActiveDPI property
    procedure ActiveDPIChanged; virtual; abstract;
  public
    constructor Create(const ADriver, ADevice, APort: string);
    destructor Destroy; override;

    function Equals(Obj: TObject): Boolean; overload; override;
    function Equals(const ADriver, ADevice, APort: string): Boolean; reintroduce; overload;
    procedure ShowDeviceOptions; virtual;

    // selects the DPI that best fits the requested DPI and activates it;
    // returns the index of the selected DPI; you can use these routines to
    // activate the best fit DPI and change the value of ActiveDPIIndex
    function SelectDPI(X, Y: Integer): Integer; overload;
    function SelectDPI(DPI: TPoint): Integer; overload; inline;

    property Device: string read FDevice;
    property Driver: string read FDriver;
    property Port: string read FPort write FPort;
    property Title: string read GetTitle;

    // the DPI that is currently active
    property ActiveDPI: TPoint read GetActiveDPI;
    // the index of the current DPI used by this printer device; you must always
    // set ActiveDPIIndex before using the printer device for printing;
    // a value of -1 will use the last selected DPI; the last selected DPI when
    // starting the application is the default DPI provided by the system; some
    // platform APIs do not work when queried for the default DPI for the printer
    // device, therefore we can use the default DPI through -1 only at the start
    // of the application; the problem in this case is that, for some platforms,
    // the size of the Canvas will not be adapted to correspond to the default
    // DPI because it may not be possible to be queried from the APIs;
    // the general rule and best practice is to always set the DPI before printing;
    // manually setting ActiveDPIIndex to -1 will cause the DPI property to return (0, 0);
    // trying to set a new value to ActiveDPIIndex of the ActiveDevice while printing
    // will cause an error
    property ActiveDPIIndex: Integer read FActiveDPIIndex write SetActiveDPIIndex;
    // supported dots-per-inch for this printer device
    property DPI[Index: Integer]: TPoint read GetDPI;
    // number of supported DPI resolutions
    property DPICount: Integer read GetDPICount;
  end;

{ TPrinter }

  TPrinter = class(TAbstractPrinter)
  private type
    TPrinterDeviceList = TObjectList<TPrinterDevice>;
  protected
    FCanvas: TCanvas;
    FAborted: Boolean;
    FFonts: TStrings;
    FPrinters: TPrinterDeviceList;
    FActivePrinter: Integer;
    FTitle: string;
    FPrinting: Boolean;
    FPageNumber: Integer;

    procedure ActivePrinterChanged; virtual; abstract;
    procedure AssignTo(Dest: TPersistent); override;
    procedure DoAbortDoc; virtual; abstract;
    procedure DoBeginDoc; virtual; abstract;
    procedure DoEndDoc; virtual; abstract;
    procedure DoNewPage; virtual; abstract;
    procedure FreeFonts;
    function GetActivePrinter: TPrinterDevice;
    function GetCanvas: TCanvas; virtual; abstract;
    function GetCapabilities: TPrinterCapabilities; virtual; abstract;
    function GetCount: Integer;
    function GetFonts: TStrings;
    function GetNumCopies: Integer; virtual; abstract;
    function GetOrientation: TPrinterOrientation; virtual; abstract;
    function GetPageHeight: Integer; virtual; abstract;
    function GetPageWidth: Integer; virtual; abstract;
    function GetPrinter(Index: Integer): TPrinterDevice;
    procedure RaiseError(const Msg: string);
    procedure RefreshFonts; virtual; abstract;
    procedure RefreshPrinterDevices; virtual; abstract;
    procedure SetActivePrinter(APrinter: TPrinterDevice);
    procedure SetDefaultPrinter; virtual; abstract;
    procedure SetNumCopies(Value: Integer); virtual;
    procedure SetOrientation(Value: TPrinterOrientation); virtual;
    procedure CheckPrinting(Printing: Boolean);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Abort;
    procedure BeginDoc;
    procedure EndDoc;
    procedure NewPage;
    procedure Refresh;
    property Aborted: Boolean read FAborted;
    property ActivePrinter: TPrinterDevice read GetActivePrinter write SetActivePrinter;
    property Canvas: TCanvas read GetCanvas;
    property Capabilities: TPrinterCapabilities read GetCapabilities;
    property Copies: Integer read GetNumCopies write SetNumCopies;
    property Count: Integer read GetCount;
    property Fonts: TStrings read GetFonts;
    property Orientation: TPrinterOrientation read GetOrientation write SetOrientation;
    property PageHeight: Integer read GetPageHeight;
    property PageNumber: Integer read FPageNumber;
    property PageWidth: Integer read GetPageWidth;
    property Printing: Boolean read FPrinting;
    property Printers[Index: Integer]: TPrinterDevice read GetPrinter;
    property Title: string read FTitle write FTitle;
  end;

  TPrinterClass = class of TPrinter;

{ TPrinterDeviceDocument }

  TPrinterDeviceDocument = class(TPrinterDevice)
  protected
    procedure ActiveDPIChanged; override;
    function GetTitle: string; override;
    procedure SetDefaultDPI; virtual;
  public
    constructor Create(const AName, AExtension: string);
  end;

{ TPrinterDocument }

  TDocumentPaper = (A4, A3, A5, Letter, Legal, Custom);

  TPrinterDocument = class(TPrinter)
  private const
    PaperHeightInInches: array[TDocumentPaper.A4..TDocumentPaper.Legal] of Single = (11.69, 16.54, 8.27, 11.00, 14.00);
    PaperWidthInInches: array[TDocumentPaper.A4..TDocumentPaper.Legal] of Single = (8.27, 11.69, 5.83, 8.50, 8.50);
  private class var
    FDocumentPaper: TDocumentPaper;
    FDocumentPaperHeight: Integer;
    FDocumentPaperWidth: Integer;
  private
    FOrientation: TPrinterOrientation;
    FOwnsStream: Boolean;
    FPageHeight: Integer;
    FPageWidth: Integer;
    FSharingService: IFMXShareSheetActionsService;
    function GetActiveExtension: string;
    function GetFileTypes: string;
    function GetFilterString: string;
    procedure SetActiveExtension(const AValue: string);
  protected
    FStream: TStream;
    procedure ActivePrinterChanged; override;
    procedure DoBeginDoc; override; final;
    procedure DoBeginDocFile; virtual; abstract;
    procedure DoEndDoc; override; final;
    procedure DoEndDocFile; virtual; abstract;
    function GetCapabilities: TPrinterCapabilities; override;
    function GetNumCopies: Integer; override;
    function GetOrientation: TPrinterOrientation; override;
    function GetPageHeight: Integer; override;
    function GetPageWidth: Integer; override;
    procedure RefreshFonts; override;
    procedure SetOrientation(AValue: TPrinterOrientation); override;
    procedure SetDefaultPrinter; override;
  public const
    DefaultDPI = 72;
  public
    /// <summary>Begin document printing process.</summary>
    /// <param name="AStream">Destination stream.</param>
    /// <remarks>
    ///   When the <c>AStream</c> is set, the save dialog or sharing service (when supported) will not open, resulting
    ///   in silent background printing (this state is reset upon calling <c>EndDoc</c>). Similarly, if the <c>Title</c>
    ///   property is set to the file name, printing occurs silently in the background (this state does not reset upon
    ///   calling <c>EndDoc</c>; instead, set the <c>Title</c> property to an empty string).
    /// </remarks>
    procedure BeginDoc(const AStream: TStream = nil);
    /// <summary>Active extension for document printing driver.</summary>
    property ActiveExtension: string read GetActiveExtension write SetActiveExtension;
    /// <summary>List of extensions (e.g. ".pdf;.xps").</summary>
    property FileTypes: string read GetFileTypes;
    /// <summary>Filter string for use in dialog components (e.g. <c>TSaveDialog</c>, <c>TOpenDialog</c>).</summary>
    property FilterString: string read GetFilterString;
    /// <summary>Height of paper measured in pixels.</summary>
    class function DocumentPaperHeight: Integer;
    /// <summary>Width of paper measured in pixels.</summary>
    class function DocumentPaperWidth: Integer;
    /// <summary>Set paper type or dimensions.</summary>
    /// <param name="APaper">Paper type.</param>
    /// <param name="AWidth">Paper width in pixels.</param>
    /// <param name="AHeight">Paper height in pixels.</param>
    /// <remarks>The specified width and height will only be considered if the paper type is set to custom.</remarks>
    class procedure SetDocumentPaper(const APaper: TDocumentPaper; const AWidth: Integer = 0; const AHeight: Integer = 0);
    /// <summary>Set paper dimensions in inches.</summary>
    /// <param name="AWidth">Paper width in inches.</param>
    /// <param name="AHeight">Paper height in inches.</param>
    /// <param name="ADPI">DPI that will be applied to the dimensions.</param>
    class procedure SetDocumentPaperSizeInInches(const AWidth, AHeight: Single; const ADPI: Integer = DefaultDPI); inline;
    /// <summary>Set paper dimensions in millimeters.</summary>
    /// <param name="AWidth">Paper width in millimeters.</param>
    /// <param name="AHeight">Paper height in millimeters.</param>
    /// <param name="ADPI">DPI that will be applied to the dimensions.</param>
    class procedure SetDocumentPaperSizeInMillimeters(const AWidth, AHeight: Single; const ADPI: Integer = DefaultDPI); inline;
  end;

  TPrinterDocumentClass = class of TPrinterDocument;

  { TPrintDialog }

type

  [ComponentPlatformsAttribute(pfidWindows or pfidOSX or pfidLinux)]
  TPrintDialog = class(TCommonDialog)
  private
    FCollate: Boolean;
    FCopies: Integer;
    FMinPage: Integer;
    FMaxPage: Integer;
    FOptions: TPrintDialogOptions;
    FFromPage: Integer;
    FToPage: Integer;
    FPrintRange: TPrintRange;
    FPrintToFile: Boolean;
    procedure SetNumCopies(Value: Integer);
  protected
    function DoExecute: Boolean; override;
  published
    property Collate: Boolean read FCollate write FCollate default False;
    property Copies: Integer read FCopies write SetNumCopies default 0;
    property FromPage: Integer read FFromPage write FFromPage default 0;
    property MinPage: Integer read FMinPage write FMinPage default 0;
    property MaxPage: Integer read FMaxPage write FMaxPage default 0;
    property Options: TPrintDialogOptions read FOptions write FOptions default [];
    property PrintToFile: Boolean read FPrintToFile write FPrintToFile default False;
    property PrintRange: TPrintRange read FPrintRange write FPrintRange default TPrintRange.prAllPages;
    property ToPage: Integer read FToPage write FToPage default 0;
  end;

{ TPrinterSetupDialog }

  [ComponentPlatformsAttribute(pfidWindows or pfidOSX or pfidLinux)]
  TPrinterSetupDialog = class(TCommonDialog)
  protected
    function DoExecute: Boolean; override;
  end;

{ TPageSetupDialog }

  TPageSetupPaintingEvent = procedure (Sender: TObject; const PaperSize: SmallInt;
    const Orientation: TPrinterOrientation; const PageType: TPageType;
    var DoneDrawing: Boolean) of object;
  TPaintPageEvent = procedure(Sender: TObject; Canvas: TCanvas; PageRect: TRect;
    var DoneDrawing: Boolean) of object;

  [ComponentPlatformsAttribute(pfidWindows or pfidOSX or pfidLinux)]
  TPageSetupDialog = class(TCommonDialog)
  private
    FOptions: TPageSetupDialogOptions;
    FMargin: TRect;
    FMinMargin: TRect;
    FPaperSize: TPointF;
    // FMinMarginLeft: Integer;
    // FMinMarginTop: Integer;
    // FMinMarginRight: Integer;
    // FMinMarginBottom: Integer;
    // FMarginLeft: Integer;
    // FMarginTop: Integer;
    // FMarginRight: Integer;
    // FMarginBottom: Integer;
    // FPageWidth: Integer;
    // FPageHeight: Integer;
    FPainting: TPageSetupPaintingEvent;
    FUnits: TPageMeasureUnits;
    FOnDrawRetAddress: TPaintPageEvent;
    FOnDrawMinMargin: TPaintPageEvent;
    FOnDrawEnvStamp: TPaintPageEvent;
    FOnDrawFullPage: TPaintPageEvent;
    FOnDrawGreekText: TPaintPageEvent;
    FOnDrawMargin: TPaintPageEvent;
    function getMinMarginLeft: Integer;
    function getMinMarginTop: Integer;
    function getMinMarginRight: Integer;
    function getMinMarginBottom: Integer;
    function getMarginLeft: Integer;
    function getMarginTop: Integer;
    function getMarginRight: Integer;
    function getMarginBottom: Integer;
    function getPageWidth: Single;
    function getPageHeight: Single;
    procedure setMinMarginLeft(Value: Integer);
    procedure setMinMarginTop(Value: Integer);
    procedure setMinMarginRight(Value: Integer);
    procedure setMinMarginBottom(Value: Integer);
    procedure setMarginLeft(Value: Integer);
    procedure setMarginTop(Value: Integer);
    procedure setMarginRight(Value: Integer);
    procedure setMarginBottom(Value: Integer);
    procedure setPageWidth(Value: Single);
    procedure setPageHeight(Value: Single);
  protected
    function DoExecute: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property MinMarginLeft: Integer read getMinMarginLeft write setMinMarginLeft;
    property MinMarginTop: Integer read getMinMarginTop write setMinMarginTop;
    property MinMarginRight: Integer read getMinMarginRight write setMinMarginRight;
    property MinMarginBottom: Integer read getMinMarginBottom write setMinMarginBottom;
    property MarginLeft: Integer read getMarginLeft write setMarginLeft;
    property MarginTop: Integer read getMarginTop write setMarginTop;
    property MarginRight: Integer read getMarginRight write setMarginRight;
    property MarginBottom: Integer read getMarginBottom write setMarginBottom;
    property Options: TPageSetupDialogOptions read FOptions write FOptions
      default [TPageSetupDialogOption.psoDefaultMinMargins];
    property PageWidth: Single read getPageWidth write setPageWidth;
    property PageHeight: Single read getPageHeight write setPageHeight;
    property Units: TPageMeasureUnits read FUnits write FUnits default TPageMeasureUnits.pmDefault;
    property Painting: TPageSetupPaintingEvent read FPainting write FPainting;
    property OnDrawFullPage: TPaintPageEvent read FOnDrawFullPage write FOnDrawFullPage;
    property OnDrawMinMargin: TPaintPageEvent read FOnDrawMinMargin write FOnDrawMinMargin;
    property OnDrawMargin: TPaintPageEvent read FOnDrawMargin write FOnDrawMargin;
    property OnDrawGreekText: TPaintPageEvent read FOnDrawGreekText write FOnDrawGreekText;
    property OnDrawEnvStamp: TPaintPageEvent read FOnDrawEnvStamp write FOnDrawEnvStamp;
    property OnDrawRetAddress: TPaintPageEvent read FOnDrawRetAddress write FOnDrawRetAddress;
  end;

function Printer: TPrinter;
function PrinterAssigned: Boolean;
function SetPrinter(const ANewPrinter: TPrinter): TPrinter;

function PrinterClass: TPrinterClass;
procedure SetPrinterClass(const APrinterClass: TPrinterClass);

function PrinterDocumentClass: TPrinterDocumentClass;
procedure SetPrinterDocumentClass(const APrinterDocumentClass: TPrinterDocumentClass);

{ AssignPrn - Assigns a Text variable to the currently selected printer.  Any
  Write or Writeln's going to that file variable will be written on the
  printer using the Canvas property's font.  A new page is automatically
  started if a CR is encountered on (or a Writeln is written to) the last
  line on the page.  Closing the text file will imply a call to the
  Printer.EndDoc method. Note: only one Text variable can be open on the
  printer at a time.  Opening a second will cause an exception. }

procedure AssignPrn(var F: Text);

implementation

uses
  FMX.Platform, FMX.Consts, System.Character
{$IFDEF MSWINDOWS}
  , FMX.Printer.Win, FMX.Canvas.GDIP
{$ENDIF}
{$IFDEF OSX}
  , FMX.Printer.Mac
{$ENDIF OSX}
{$IFDEF IOS}
  , FMX.Printer.iOS
{$ENDIF IOS}
{$IFDEF ANDROID}
  , FMX.Printer.Android
{$ENDIF ANDROID}
{$IFDEF LINUX}
  , FMX.Printer.Linux
{$ENDIF LINUX}
;

var
  GPrinter: TPrinter;
  GPrinterClass: TPrinterClass;
  GPrinterDocumentClass: TPrinterDocumentClass;

function Printer: TPrinter;
begin
  if (GPrinter = nil) and (PrinterClass <> nil) then
    GPrinter := PrinterClass.Create;
  Result := GPrinter;
end;

function PrinterAssigned: Boolean;
begin
  Result := GPrinter <> nil;
end;

function SetPrinter(const ANewPrinter: TPrinter): TPrinter;
begin
  Result := GPrinter;
  GPrinter := ANewPrinter;
end;

function PrinterClass: TPrinterClass;
begin
  if GPrinterClass <> nil then
    Result := GPrinterClass
  else if GPrinterDocumentClass <> nil then
    Result := GPrinterDocumentClass
  else
    Result := ActualPrinterClass
end;

procedure SetPrinterClass(const APrinterClass: TPrinterClass);
begin
  GPrinterClass := APrinterClass;
  FreeAndNil(GPrinter);
end;

function PrinterDocumentClass: TPrinterDocumentClass;
begin
  Result := GPrinterDocumentClass;
end;

procedure SetPrinterDocumentClass(const APrinterDocumentClass: TPrinterDocumentClass);
begin
  GPrinterDocumentClass := APrinterDocumentClass;
end;

{ TPrinterDevice }

constructor TPrinterDevice.Create(const ADriver, ADevice, APort: string);
begin
  inherited Create;
  FDriver := ADriver;
  FDevice := ADevice;
  FPort := APort;
  FActiveDPIIndex := -1;
  FDPIList := TList<TPoint>.Create;
  RefreshDPIList;
end;

function TPrinterDevice.Equals(Obj: TObject): Boolean;
begin
  Result := (Obj is TPrinterDevice) and SameText(TPrinterDevice(Obj).Device, Device) and
    SameText(TPrinterDevice(Obj).Driver, Driver) and SameText(TPrinterDevice(Obj).Port, Port);
end;

destructor TPrinterDevice.Destroy;
begin
  FreeAndNil(FDPIList);
  inherited;
end;

procedure TPrinterDevice.DPIChangeError;
begin
  raise EPrinterSettingsError.Create(SPrinterDPIChangeError);
end;

function TPrinterDevice.Equals(const ADriver, ADevice, APort: string): Boolean;
begin
  Result := (Device = ADevice) and ((Port = '') or (Port = APort));
end;

function TPrinterDevice.GetActiveDPI: TPoint;
begin
  Result := DPI[ActiveDPIIndex];
end;

function TPrinterDevice.GetDPI(Index: Integer): TPoint;
begin
  if Index = -1 then
    Result := TPoint.Zero
  else
    Result := FDPIList[Index];
end;

function TPrinterDevice.GetDPICount: Integer;
begin
  Result := FDPIList.Count;
end;

function TPrinterDevice.GetTitle: string;
begin
  Result := Format(sDeviceOnPort, [FDevice, FPort]);
end;

function TPrinterDevice.SelectDPI(X, Y: Integer): Integer;
var
  Area: Integer;
  MinArea: Integer;
  MinIndex: Integer;
  i: Integer;
begin
  MinArea := MaxInt;
  MinIndex := -1;
  // calculate the DPI rectangle that covers most of the requeste DPI rectangle
  for i := 0 to DPICount - 1 do
  begin
    Area := Abs(X - DPI[i].X) * Abs(Y - DPI[i].Y);
    if MinArea > Area then
    begin
      MinArea := Area;
      MinIndex := i;
    end;
  end;

  ActiveDPIIndex := MinIndex;
  Result := MinIndex;
end;

function TPrinterDevice.SelectDPI(DPI: TPoint): Integer;
begin
  Result := SelectDPI(DPI.X, DPI.Y);
end;

procedure TPrinterDevice.SetActiveDPIIndex(const Value: Integer);
begin
  if FActiveDPIIndex <> Value then
  begin
    // can't set new DPI values while printing
    if (Printer.Printing) and (Self = Printer.ActivePrinter) then
      DPIChangeError;

    FActiveDPIIndex := Value;
    if FActiveDPIIndex <> -1 then
      ActiveDPIChanged;
  end;
end;

procedure TPrinterDevice.ShowDeviceOptions;
begin
  // Descendants can display the options dialog for printer device
end;

{ TPrinter }

constructor TPrinter.Create;
begin
  inherited Create;
  FPrinters := TPrinterDeviceList.Create(True);
  FActivePrinter := -1;
end;

destructor TPrinter.Destroy;
begin
  if Printing then
    DoEndDoc;
  FreeAndNil(FPrinters);
  FreeAndNil(FFonts);
  FreeAndNil(FCanvas);
  inherited Destroy;
end;

procedure TPrinter.Abort;
begin
  CheckPrinting(True);
  DoAbortDoc;
  FAborted := True;
  EndDoc;
  FAborted := True;
end;

procedure TPrinter.AssignTo(Dest: TPersistent);
var
  LDest: TStrings;
  I: Integer;
  Dev: TPrinterDevice;
begin
  if Dest is TStrings then
  begin
    LDest := TStrings(Dest);
    LDest.Clear;
    for I := 0 to Count - 1 do
    begin
      Dev := Printers[I];
      LDest.AddObject(Dev.Title, Dev);
    end;
  end
  else
    inherited AssignTo(Dest);
end;

procedure TPrinter.BeginDoc;
begin
  CheckPrinting(False);
  DoBeginDoc;
  FPrinting := True;
  FAborted := False;
  FPageNumber := 1;
end;

procedure TPrinter.CheckPrinting(Printing: Boolean);
begin
  if Self.Printing <> Printing then
    if Printing then
      RaiseError(SNotPrinting)
    else
      RaiseError(SPrinting);
end;

procedure TPrinter.EndDoc;
begin
  CheckPrinting(True);
  DoEndDoc;
  FPrinting := False;
  FAborted := False;
  FPageNumber := 0;
end;

procedure TPrinter.FreeFonts;
begin
  FreeAndNil(FFonts);
end;

function TPrinter.GetActivePrinter: TPrinterDevice;
begin
  if FActivePrinter = -1 then
  begin
    if FPrinters.Count = 0 then
    begin
      RefreshPrinterDevices;
      if FPrinters.Count <> 0 then
        SetDefaultPrinter;
    end;
  end;
  Result := GetPrinter(FActivePrinter);
end;

function TPrinter.GetCount: Integer;
begin
  if FPrinters.Count = 0 then
  begin
    RefreshPrinterDevices;
    // Only call SetDefaultPrinter if there are printers instaled.
    if FPrinters.Count <> 0 then
      SetDefaultPrinter;
  end;
  Result := FPrinters.Count;
end;

function TPrinter.GetFonts: TStrings;
begin
  if FFonts = nil then
  try
    FFonts := TStringList.Create;
    RefreshFonts;
  except
    FreeAndNil(FFonts);
    raise;
  end;
  Result := FFonts;
end;

function TPrinter.GetPrinter(Index: Integer): TPrinterDevice;
begin
  if Index < 0 then
    RaiseError(SPrinterIndexError);
  if FPrinters.Count = 0 then
  begin
    RefreshPrinterDevices;
    SetDefaultPrinter;
  end;
  if Index >= FPrinters.Count then
    RaiseError(SPrinterIndexError);
  Result := FPrinters[Index];
end;

procedure TPrinter.NewPage;
begin
  CheckPrinting(True);
  DoNewPage;
  Inc(FPageNumber);
end;

procedure TPrinter.RaiseError(const Msg: string);
begin
  raise EPrinter.Create(Msg);
end;

procedure TPrinter.Refresh;
begin
  FreeFonts;
  FPrinters.Clear;
end;

procedure TPrinter.SetActivePrinter(APrinter: TPrinterDevice);
var
  NewIndex: Integer;
  Changed: Boolean;
begin
  CheckPrinting(False);
  if (FPrinters <> nil) and (APrinter <> nil) then
  begin
    NewIndex := FPrinters.IndexOf(APrinter);
    if NewIndex > -1 then
    begin
      Changed := FActivePrinter <> NewIndex;
      FActivePrinter := NewIndex;
      if Changed then
        ActivePrinterChanged;
    end else
      RaiseError(sInvalidPrinter);
  end;
end;

procedure TPrinter.SetNumCopies(Value: Integer);
begin
  CheckPrinting(False);
end;

procedure TPrinter.SetOrientation(Value: TPrinterOrientation);
begin
  CheckPrinting(False);
end;

{ TPrinterDeviceDocument }

procedure TPrinterDeviceDocument.ActiveDPIChanged;
begin
end;

constructor TPrinterDeviceDocument.Create(const AName, AExtension: string);
var
  LExtension: string;
begin
  LExtension := AExtension.Trim;
  if not LExtension.StartsWith('.') then
    LExtension := '.' + LExtension;
  inherited Create(AName, LExtension, '');
  if FDPIList.Count > 0 then
    SetDefaultDPI;
end;

function TPrinterDeviceDocument.GetTitle: string;
begin
  Result := Format('%s Documents (*%s)|*%s', [FDriver, FDevice, FDevice]);
end;

procedure TPrinterDeviceDocument.SetDefaultDPI;
begin
  SelectDPI(TPrinterDocument.DefaultDPI, TPrinterDocument.DefaultDPI);
end;

{ TPrinterDocument }

procedure TPrinterDocument.ActivePrinterChanged;
begin
end;

procedure TPrinterDocument.BeginDoc(const AStream: TStream);
begin
  FStream := AStream;
  inherited BeginDoc;
end;

procedure TPrinterDocument.DoBeginDoc;
begin
  FOwnsStream := FStream = nil;
  if FOwnsStream then
  begin
    if not FTitle.IsEmpty then
      FStream := TFileStream.Create(FTitle, fmCreate)
    else if (FSharingService <> nil) or TPlatformServices.Current.SupportsPlatformService(IFMXShareSheetActionsService, FSharingService) then
      FStream := TMemoryStream.Create
    else
    begin
      var LSaveDialog := TSaveDialog.Create(nil);
      try
        LSaveDialog.Filter := GetFilterString;
        LSaveDialog.DefaultExt := GetActiveExtension.Substring(1);
        LSaveDialog.Options := LSaveDialog.Options + [TOpenOption.ofOverwritePrompt];
        if not LSaveDialog.Execute then
          FStream := TMemoryStream.Create // NoOp
        else
        begin
          SetActiveExtension(TPath.GetExtension(LSaveDialog.FileName));
          FStream := TFileStream.Create(LSaveDialog.FileName, fmCreate);
        end;
      finally
        LSaveDialog.Free;
      end;
    end;
  end;
  FPageWidth := GetPageWidth;
  FPageHeight := GetPageHeight;
  DoBeginDocFile;
end;

class function TPrinterDocument.DocumentPaperHeight: Integer;
begin
  if FDocumentPaper = TDocumentPaper.Custom then
    Result := FDocumentPaperHeight
  else
    Result := Round(PaperHeightInInches[FDocumentPaper] * DefaultDPI);
end;

class function TPrinterDocument.DocumentPaperWidth: Integer;
begin
  if FDocumentPaper = TDocumentPaper.Custom then
    Result := FDocumentPaperWidth
  else
    Result := Round(PaperWidthInInches[FDocumentPaper] * DefaultDPI);
end;

procedure TPrinterDocument.DoEndDoc;
begin
  DoEndDocFile;
  if FOwnsStream then
  begin
    if FTitle.IsEmpty and (FSharingService <> nil) then
      FSharingService.Share(nil, '', [FStream], [GetActiveExtension]);
    FStream.Free;
  end;
  FStream := nil;
end;

function TPrinterDocument.GetActiveExtension: string;
begin
  Result := GetActivePrinter.Device;
end;

function TPrinterDocument.GetCapabilities: TPrinterCapabilities;
begin
  Result := [TPrinterCapability.pcOrientation];
end;

function TPrinterDocument.GetFileTypes: string;
begin
  Result := '';
  for var I := 0 to GetCount - 1 do
  begin
    if not Result.IsEmpty then
      Result := Result + ';';
    Result := Result + '*' + GetPrinter(I).Device;
  end;
end;

function TPrinterDocument.GetFilterString: string;
var
  LFileTypes: string;
begin
  Result := '';
  for var I := 0 to GetCount - 1 do
  begin
    if not Result.IsEmpty then
      Result := Result + '|';
    Result :=  Result + GetPrinter(I).Title;
  end;
  LFileTypes := GetFileTypes;
  Result := SVAllFiles + ' (' + LFileTypes + ')|' + LFileTypes + '|' + Result;
end;

function TPrinterDocument.GetNumCopies: Integer;
begin
  Result := 1;
end;

function TPrinterDocument.GetOrientation: TPrinterOrientation;
begin
  Result := FOrientation;
end;

function TPrinterDocument.GetPageHeight: Integer;
begin
  if FPrinting then
    Result := FPageHeight
  else if FOrientation = TPrinterOrientation.poPortrait then
    Result := DocumentPaperHeight
  else
    Result := DocumentPaperWidth;
end;

function TPrinterDocument.GetPageWidth: Integer;
begin
  if FPrinting then
    Result := FPageWidth
  else if FOrientation = TPrinterOrientation.poPortrait then
    Result := DocumentPaperWidth
  else
    Result := DocumentPaperHeight;
end;

procedure TPrinterDocument.RefreshFonts;
begin
end;

procedure TPrinterDocument.SetActiveExtension(const AValue: string);
var
  LExtension: string;
begin
  CheckPrinting(False);
  LExtension := AValue.Trim;
  if not LExtension.StartsWith('.') then
    LExtension := '.' + LExtension;
  for var I := 0 to GetCount - 1 do
    if SameText(LExtension, GetPrinter(I).Device, loUserLocale) then
    begin
      SetActivePrinter(GetPrinter(I));
      Exit;
    end;
  RaiseError(sInvalidPrinter);
end;

procedure TPrinterDocument.SetDefaultPrinter;
begin
  FActivePrinter := 0;
end;

class procedure TPrinterDocument.SetDocumentPaper(const APaper: TDocumentPaper; const AWidth, AHeight: Integer);
begin
  FDocumentPaper := APaper;
  FDocumentPaperWidth := AWidth;
  FDocumentPaperHeight := AHeight;
end;

class procedure TPrinterDocument.SetDocumentPaperSizeInInches(const AWidth, AHeight: Single; const ADPI: Integer);
begin
  SetDocumentPaper(TDocumentPaper.Custom, Round(AWidth * ADPI), Round(AHeight * ADPI));
end;

class procedure TPrinterDocument.SetDocumentPaperSizeInMillimeters(const AWidth, AHeight: Single; const ADPI: Integer);
begin
  SetDocumentPaperSizeInInches(AWidth / 25.4, AHeight / 25.4, ADPI);
end;

procedure TPrinterDocument.SetOrientation(AValue: TPrinterOrientation);
begin
  inherited;
  FOrientation := AValue;
end;

{ TPrintDialog }

function TPrintDialog.DoExecute: Boolean;
var
  DialogService: IFMXDialogService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXDialogService, DialogService) then
    Result := DialogService.DialogPrint(FCollate, FPrintToFile, FFromPage, FToPage, FCopies,
      FMinPage, FMaxPage, FPrintRange, FOptions)
  else
    Result := False;
end;

procedure TPrintDialog.SetNumCopies(Value: Integer);
begin
  FCopies := Value;
  Printer.Copies := Value;
end;

{ TPageSetupDialog }

constructor TPageSetupDialog.Create(AOwner: TComponent);
var
  DialogService: IFMXDialogService;
begin
  inherited;
  Options := [TPageSetupDialogOption.psoDefaultMinMargins];
  if TPlatformServices.Current.SupportsPlatformService(IFMXDialogService, DialogService) then
    DialogService.PageSetupGetDefaults(FMargin, FMinMargin, FPaperSize, FUnits, FOptions);
end;

function TPageSetupDialog.DoExecute: Boolean;
var
  DialogService: IFMXDialogService;
begin
  Result := False;
  if TPlatformServices.Current.SupportsPlatformService(IFMXDialogService, DialogService) then
    Result := DialogService.DialogPageSetup(FMargin, FMinMargin, FPaperSize, FUnits, FOptions);
end;

function TPageSetupDialog.getMarginBottom: Integer;
begin
  Result := FMargin.Bottom;
end;

function TPageSetupDialog.getMarginLeft: Integer;
begin
  Result := FMargin.Left;
end;

function TPageSetupDialog.getMarginRight: Integer;
begin
  Result := FMargin.Right;
end;

function TPageSetupDialog.getMarginTop: Integer;
begin
  Result := FMargin.Top;
end;

function TPageSetupDialog.getMinMarginBottom: Integer;
begin
  Result := FMinMargin.Bottom;
end;

function TPageSetupDialog.getMinMarginLeft: Integer;
begin
  Result := FMinMargin.Left;
end;

function TPageSetupDialog.getMinMarginRight: Integer;
begin
  Result := FMinMargin.Right;
end;

function TPageSetupDialog.getMinMarginTop: Integer;
begin
  Result := FMinMargin.Top;
end;

function TPageSetupDialog.getPageHeight: Single;
begin
  Result := FPaperSize.Y;
end;

function TPageSetupDialog.getPageWidth: Single;
begin
  Result := FPaperSize.X;
end;

procedure TPageSetupDialog.setMarginBottom(Value: Integer);
begin
  if Value <> FMargin.Bottom then
    if Value >= 0 then
      FMargin.Bottom := Value;
end;

procedure TPageSetupDialog.setMarginLeft(Value: Integer);
begin
  if Value <> FMargin.Left then
    if Value >= 0 then
      FMargin.Left := Value;
end;

procedure TPageSetupDialog.setMarginRight(Value: Integer);
begin
  if Value <> FMargin.Right then
    if Value >= 0 then
      FMargin.Right := Value;
end;

procedure TPageSetupDialog.setMarginTop(Value: Integer);
begin
  if Value <> FMargin.Top then
    if Value >= 0 then
      FMargin.Top := Value;
end;

procedure TPageSetupDialog.setMinMarginBottom(Value: Integer);
begin
  if Value <> FMinMargin.Bottom then
    if Value >= 0 then
      FMinMargin.Bottom := Value;
end;

procedure TPageSetupDialog.setMinMarginLeft(Value: Integer);
begin
  if Value <> FMinMargin.Left then
    if Value >= 0 then
      FMinMargin.Left := Value;
end;

procedure TPageSetupDialog.setMinMarginRight(Value: Integer);
begin
  if Value <> FMinMargin.Right then
    if Value >= 0 then
      FMinMargin.Right := Value;
end;

procedure TPageSetupDialog.setMinMarginTop(Value: Integer);
begin
  if Value <> FMinMargin.Top then
    if Value >= 0 then
      FMinMargin.Top := Value;
end;

procedure TPageSetupDialog.setPageHeight(Value: Single);
begin
  if Value <> FPaperSize.Y then
    if Value >= 0 then
      FPaperSize.Y := Value;
end;

procedure TPageSetupDialog.setPageWidth(Value: Single);
begin
  if Value <> FPaperSize.X then
    if Value >= 0 then
      FPaperSize.X := Value;
end;

{ TPrinterSetupDialog }

function TPrinterSetupDialog.DoExecute: Boolean;
var
  DialogService: IFMXDialogService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXDialogService, DialogService) then
    Result := DialogService.DialogPrinterSetup
  else
    Result := False;
end;

{ AssignPrn support }

type
  PrnRec = record
    case Integer of
      1:
        (Cur: TPointF;
         Finish: TPointF; { End of the printable area }
         Height: Single;  { Height of the current line }
         CodePage: Word); { CodePage to use on incoming data }
      2:
        (Tmp: array [1..32] of Byte
        );
  end;

procedure NewPage(var Prn: PrnRec);
begin
  Prn.Cur.X := 0;
  Prn.Cur.Y := 0;
  Printer.NewPage;
end;

{ Start a new line on the current page, if no more lines left start a new
  page. }
procedure NewLine(var Prn: PrnRec);

  function CharHeight: Word;
  begin
    Result := Trunc(Printer.Canvas.TextHeight('Ij')); // do not localize;
  end;

begin
  Prn.Cur.X := 0;
  if Prn.Height = 0 then
    Prn.Cur.Y := Prn.Cur.Y + CharHeight
  else
    Prn.Cur.Y := Prn.Cur.Y + Prn.Height;
  if Prn.Cur.Y > (Prn.Finish.Y - (Prn.Height * 2)) then
    NewPage(Prn);
  Prn.Height := 0;
end;

{ Print a string to the printer without regard to special characters.  These
  should handled by the caller. }

procedure PrnOutStr(var Prn: PrnRec; Text: MarshaledAString; Len: Integer);
var
  R: TRectF;
  Extent: TSizeF;
  L: Integer;
  S: string;
  LText: PChar;

  function CharPrev(Start, Current: PChar): PChar;
  begin
    if Current <> Start then
    begin
      if Current^.IsSurrogate then
        Result := Current - 2
      else
        Result := Current - 1;
    end
    else
      Result := Start;
  end;

  procedure TextExtent(Text: PChar; Len: Integer; var R: TRectF);
  var
    S: string;
  begin
    SetString(S, Text, Len);
    Printer.Canvas.MeasureText(R, S, False, [], TTextAlign.Leading);
  end;

begin
  L := UnicodeFromLocaleChars(Prn.CodePage, 0, Text, Len, nil, 0);
  SetLength(S, L);
  Len := UnicodeFromLocaleChars(Prn.CodePage, 0, Text, Len, PChar(S), Length(S));
  LText := PChar(S);
  while Len > 0 do
  begin
    L := Len;
    R := TRectF.Empty;
    TextExtent(LText, L, R);
  //Keep the width and the height calculated by TextExtent, but place the rectangle
  //where we need it to be (modify the starting point).
    R.SetLocation(Prn.Cur.X, Prn.Cur.Y);
    Extent := R.Size;

    while (L > 0) and (Extent.cX + Prn.Cur.X > Prn.Finish.X) do
    begin
      L := CharPrev(LText, LText + L) - LText;
      TextExtent(LText, L, R);
      Extent := R.Size;
    end;

    if Extent.cY > Prn.Height then
      Prn.Height := Extent.cY + 2;
    SetString(S, LText, L);
    Printer.Canvas.FillText(R, S, False, 1, [], TTextAlign.Leading);
    Dec(Len, L);
    Inc(LText, L);
    if Len > 0 then
      NewLine(Prn)
    else
      Prn.Cur.X := Prn.Cur.X + Extent.cX;
  end;
end;

{ Print a string to the printer handling special characters. }
procedure PrnString(var Prn: PrnRec; Text: MarshaledAString; Len: Integer);
var
  L: Integer;
  TabWidth: Single;

  procedure Flush;
  begin
    if L <> 0 then
      PrnOutStr(Prn, Text, L);
    Inc(Text, L + 1);
    Dec(Len, L + 1);
    L := 0;
  end;

  function AvgCharWidth: Single;
  begin
    Result := Printer.Canvas.TextWidth('ABCDEFGHIJKLMNOPQRSTUVWXYZ') / 26; // do not localize
  end;

begin
  L := 0;
  while L < Len do
  begin
    case Text[L] of
      #9:
        begin
          Flush;
          TabWidth := AvgCharWidth * 8;
          Prn.Cur.X := Prn.Cur.X + TabWidth - ((Round(Prn.Cur.X + TabWidth + 1) mod Round(TabWidth)) + 1);
          if Prn.Cur.X > Prn.Finish.X then
            NewLine(Prn);
        end;
      #13:
        Flush;
      #10:
        begin
          Flush;
          NewLine(Prn);
        end;
      ^L:
        begin
          Flush;
          NewPage(Prn);
        end;
    else
      Inc(L);
    end;
  end;
  Flush;
end;

{ Called when a Read or Readln is applied to a printer file. Since reading is
  illegal this routine tells the I/O system that no characters where read, which
  generates a runtime error. }
function PrnInput(var F: TTextRec): Integer;
begin
  F.BufPos := 0;
  F.BufEnd := 0;
  Result := 0;
end;

{ Called when a Write or Writeln is applied to a printer file. The calls
  PrnString to write the text in the buffer to the printer. }
function PrnOutput(var F: TTextRec): Integer;
begin
  PrnString(PrnRec(F.UserData), MarshaledAString(F.BufPtr), F.BufPos);
  F.BufPos := 0;
  Result := 0;
end;

{ Will ignore certain requests by the I/O system such as flush while doing an
  input. }
function PrnIgnore(var F: TTextRec): Integer;
begin
  Result := 0;
end;

{ Deallocates the resources allocated to the printer file. }
function PrnClose(var F: TTextRec): Integer;
begin
//  PrnRec(F.UserData)
  Printer.EndDoc;
  Result := 0;
end;

{ Called to open I/O on a printer file.  Sets up the TTextFile to point to
  printer I/O functions. }
function PrnOpen(var F: TTextRec): Integer;
var
  Prn: PrnRec;
begin
  if F.Mode = fmInput then
  begin
    F.InOutFunc := @PrnInput;
    F.FlushFunc := @PrnIgnore;
    F.CloseFunc := @PrnIgnore;
  end
  else
  begin
    Prn := PrnRec(F.UserData);
    F.Mode := fmOutput;
    F.InOutFunc := @PrnOutput;
    F.FlushFunc := @PrnOutput;
    F.CloseFunc := @PrnClose;
    Printer.BeginDoc;

    Printer.Canvas.Font.Family := 'Courier New';
    Printer.Canvas.Fill.Kind := TBrushKind.Solid;
    Printer.Canvas.Fill.Color := claBlack;

    Prn.Cur.X := 0;
    Prn.Cur.Y := 0;
    Prn.Finish.X := Printer.PageWidth;
    Prn.Finish.Y := Printer.PageHeight;
    Prn.Height := 0;
    Prn.CodePage := F.CodePage;
    TArray.Copy<Byte>(Prn.Tmp, F.UserData, Length(Prn.Tmp));
  end;
  Result := 0;
end;

procedure AssignPrn(var F: Text);
begin
  FillChar(F, SizeOf(F), 0);
  TTextRec(F).Mode := fmClosed;
  TTextRec(F).BufSize := SizeOf(TTextRec(F).Buffer);
  TTextRec(F).BufPtr := @(TTextRec(F).Buffer);
  TTextRec(F).OpenFunc := @PrnOpen;
  Printer;
end;

initialization

finalization
  FreeAndNil(GPrinter);

end.
