{*******************************************************}
{                                                       }
{            RadStudio Debugger Visualizer Sample       }
{ Copyright(c) 2009-2024 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit StringListVisualizer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ToolsAPI, Vcl.StdCtrls;

type
  TAvailableState = (asAvailable, asProcRunning, asOutOfScope, asNotAvailable);

  TStringListViewerFrame = class(TFrame, IOTADebuggerVisualizerExternalViewerUpdater,
    IOTAThreadNotifier, IOTAThreadNotifier160)
    PCViews: TPageControl;
    TabList: TTabSheet;
    TabText: TTabSheet;
    StringListView: TListView;
    StringTextView: TMemo;
    procedure StringListViewData(Sender: TObject; Item: TListItem);
    procedure PCViewsChange(Sender: TObject);
    procedure StringListViewDrawItem(Sender: TCustomListView; Item: TListItem;
      Rect: TRect; State: TOwnerDrawState);
  private
    FOwningForm: TCustomForm;
    FClosedProc: TOTAVisualizerClosedProcedure;
    FExpression: string;
    FNotifierIndex: Integer;
    FCompleted: Boolean;
    FDeferredResult: string;
    FDeferredError: Boolean;
    FItems: TStrings;
    FAvailableState: TAvailableState;
    function Evaluate(Expression: string): string;
    function GetDelimiter: string;
    function GetStrictDelimiter: Boolean;
    function GetDelimitedText: string;
    function GetText: string;
    procedure GetSaveToFile(const AFileName: string);
    function FromDbgStrToText(const AText: string): string;
  protected
    procedure SetParent(AParent: TWinControl); override;
  public
    procedure CloseVisualizer;
    procedure MarkUnavailable(Reason: TOTAVisualizerUnavailableReason);
    procedure RefreshVisualizer(const Expression, TypeName, EvalResult: string);
    procedure SetClosedCallback(ClosedProc: TOTAVisualizerClosedProcedure);
    procedure SetForm(AForm: TCustomForm);
    procedure AddStringListItems(const Expression, TypeName, EvalResult: string);

    { IOTAThreadNotifier }
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    procedure ThreadNotify(Reason: TOTANotifyReason);
    procedure EvaluateComplete(const ExprStr, ResultStr: string; CanModify: Boolean;
      ResultAddress, ResultSize: LongWord; ReturnCode: Integer); overload;
    procedure ModifyComplete(const ExprStr, ResultStr: string; ReturnCode: Integer);
    { IOTAThreadNotifier160 }
    procedure EvaluateComplete(const ExprStr, ResultStr: string; CanModify: Boolean;
      ResultAddress: TOTAAddress; ResultSize: LongWord; ReturnCode: Integer); overload;
  end;

procedure Register;

implementation

uses
  DesignIntf, Actnlist, ImgList, Menus, IniFiles, System.IOUtils, Vcl.Themes;

{$R *.dfm}

resourcestring
  sStringListVisualizerName = 'TStrings Visualizer for Delphi';
  sStringListVisualizerDescription = 'Displays a list of the actual strings held in a TStrings instance';
  sMenuText = 'Show Strings';
  sFormCaption = 'TStrings Visualizer - %s';
  sProcessNotAccessible = 'process not accessible';
  sValueNotAccessible = 'value not accessible';
  sOutOfScope = 'out of scope';

type

  IFrameFormHelper = interface
    ['{0FD4A98F-CE6B-422A-BF13-14E59707D3B2}']
    function GetForm: TCustomForm;
    function GetFrame: TCustomFrame;
    procedure SetForm(Form: TCustomForm);
    procedure SetFrame(Form: TCustomFrame);
  end;

  TStringListVisualizerForm = class(TInterfacedObject, INTACustomDockableForm, IFrameFormHelper)
  private
    FMyFrame: TStringListViewerFrame;
    FMyForm: TCustomForm;
    FExpression: string;
  public
    constructor Create(const Expression: string);
    { INTACustomDockableForm }
    function GetCaption: string;
    function GetFrameClass: TCustomFrameClass;
    procedure FrameCreated(AFrame: TCustomFrame);
    function GetIdentifier: string;
    function GetMenuActionList: TCustomActionList;
    function GetMenuImageList: TCustomImageList;
    procedure CustomizePopupMenu(PopupMenu: TPopupMenu);
    function GetToolbarActionList: TCustomActionList;
    function GetToolbarImageList: TCustomImageList;
    procedure CustomizeToolBar(ToolBar: TToolBar);
    procedure LoadWindowState(Desktop: TCustomIniFile; const Section: string);
    procedure SaveWindowState(Desktop: TCustomIniFile; const Section: string; IsProject: Boolean);
    function GetEditState: TEditState;
    function EditAction(Action: TEditAction): Boolean;
    { IFrameFormHelper }
    function GetForm: TCustomForm;
    function GetFrame: TCustomFrame;
    procedure SetForm(Form: TCustomForm);
    procedure SetFrame(Frame: TCustomFrame);
  end;

  TDebuggerStringListVisualizer = class(TInterfacedObject, IOTADebuggerVisualizer,
    IOTADebuggerVisualizerExternalViewer)
  public
    function GetSupportedTypeCount: Integer;
    procedure GetSupportedType(Index: Integer; var TypeName: string;
      var AllDescendants: Boolean);
    function GetVisualizerIdentifier: string;
    function GetVisualizerName: string;
    function GetVisualizerDescription: string;
    function GetMenuText: string;
    function Show(const Expression, TypeName, EvalResult: string; Suggestedleft, SuggestedTop: Integer): IOTADebuggerVisualizerExternalViewerUpdater;
  end;

{ TDebuggerStringListVisualizer }

function TDebuggerStringListVisualizer.GetMenuText: string;
begin
  Result := sMenuText;
end;

procedure TDebuggerStringListVisualizer.GetSupportedType(Index: Integer;
  var TypeName: string; var AllDescendants: Boolean);
begin
  TypeName := 'TStrings';
  AllDescendants := True;
end;

function TDebuggerStringListVisualizer.GetSupportedTypeCount: Integer;
begin
  Result := 1;
end;

function TDebuggerStringListVisualizer.GetVisualizerDescription: string;
begin
  Result := sStringListVisualizerDescription;
end;

function TDebuggerStringListVisualizer.GetVisualizerIdentifier: string;
begin
  Result := ClassName;
end;

function TDebuggerStringListVisualizer.GetVisualizerName: string;
begin
  Result := sStringListVisualizerName;
end;

function TDebuggerStringListVisualizer.Show(const Expression, TypeName, EvalResult: string; SuggestedLeft, SuggestedTop: Integer): IOTADebuggerVisualizerExternalViewerUpdater;
var
  AForm: TCustomForm;
  AFrame: TStringListViewerFrame;
  VisDockForm: INTACustomDockableForm;
  LThemingServices: IOTAIDEThemingServices;
begin
  VisDockForm := TStringListVisualizerForm.Create(Expression) as INTACustomDockableForm;
  AForm := (BorlandIDEServices as INTAServices).CreateDockableForm(VisDockForm);
  AForm.Left := SuggestedLeft;
  AForm.Top := SuggestedTop;
  if Supports(BorlandIDEServices, IOTAIDEThemingServices, LThemingServices) and
    LThemingServices.IDEThemingEnabled then
    LThemingServices.ApplyTheme(AForm);
  (VisDockForm as IFrameFormHelper).SetForm(AForm);
  AFrame := (VisDockForm as IFrameFormHelper).GetFrame as TStringListViewerFrame;
  AFrame.AddStringListItems(Expression, TypeName, EvalResult);
  Result := AFrame as IOTADebuggerVisualizerExternalViewerUpdater;
end;


{ TStringListViewerFrame }

function TStringListViewerFrame.FromDbgStrToText(const AText: string): string;
var
  LStream: TStream;
  LParser: TParser;
  LBytes: TBytes;
begin
  LStream := TMemoryStream.Create;
  try
    LBytes := TEncoding.UTF8.GetPreamble;
    LStream.Write(LBytes, Length(LBytes));

    LBytes := TEncoding.UTF8.GetBytes(AText);
    LStream.Write(LBytes, Length(LBytes));

    LStream.Position := 0;
    LParser := TParser.Create(LStream);
    try
      Result := LParser.TokenString;
    finally
      LParser.Free;
    end;
  finally
    LStream.Free;
  end;
end;

procedure TStringListViewerFrame.AddStringListItems(const Expression, TypeName,
  EvalResult: string);
var
  Delim, DelimText, Txt, TempFile: string;
  Done: Boolean;
begin
  FAvailableState := asAvailable;
  FExpression := Expression;
  if FItems = nil then
    FItems := TStringList.Create
  else
    FItems.Clear;

  Delim := GetDelimiter;
  if Length(Delim) > 1 then
  begin
    FItems.Delimiter := Delim[2];
    FItems.StrictDelimiter := GetStrictDelimiter;
    DelimText := GetDelimitedText;
    if DelimText <> '' then
    begin
      FItems.DelimitedText := DelimText;
      StringListView.Items.Count := FItems.Count;
    end else
      StringListView.Items.Count := 0;
    StringListView.Invalidate;

    Txt := GetText;
    Done := False;
    if (Length(Txt) < 4096) and not Txt.EndsWith('...') then
      try
        StringTextView.Text := FromDbgStrToText(Txt);
        Done := True;
      except
      end;
    if not Done then
    begin
      TempFile := TPath.GetTempFileName;
      try
        GetSaveToFile(TempFile);
        StringTextView.Lines.LoadFromFile(TempFile, TEncoding.UTF8);
      finally
        DeleteFile(TempFile);
      end;
    end;
  end else
  begin
    FAvailableState := asNotAvailable;
    StringListView.Invalidate;
    StringTextView.Clear;
  end;
end;

procedure TStringListViewerFrame.AfterSave;
begin

end;

procedure TStringListViewerFrame.BeforeSave;
begin

end;

procedure TStringListViewerFrame.CloseVisualizer;
begin
  if FOwningForm <> nil then
    FOwningForm.Close;
end;

procedure TStringListViewerFrame.Destroyed;
begin

end;

function TStringListViewerFrame.Evaluate(Expression: string): string;
var
  CurProcess: IOTAProcess;
  CurThread: IOTAThread;
  ResultStr: array[0..4095] of Char;
  CanModify: Boolean;
  Done: Boolean;
  ResultAddr, ResultSize, ResultVal: LongWord;
  EvalRes: TOTAEvaluateResult;
  DebugSvcs: IOTADebuggerServices;
begin
  begin
    Result := '';
    if Supports(BorlandIDEServices, IOTADebuggerServices, DebugSvcs) then
      CurProcess := DebugSvcs.CurrentProcess;
    if CurProcess <> nil then
    begin
      CurThread := CurProcess.CurrentThread;
      if CurThread <> nil then
      begin
        repeat
        begin
          Done := True;
          EvalRes := CurThread.Evaluate(Expression, @ResultStr, Length(ResultStr),
            CanModify, eseAll, '', ResultAddr, ResultSize, ResultVal, '', 0);
          case EvalRes of
            erOK: Result := ResultStr;
            erDeferred:
              begin
                FCompleted := False;
                FDeferredResult := '';
                FDeferredError := False;
                FNotifierIndex := CurThread.AddNotifier(Self);
                while not FCompleted do
                  DebugSvcs.ProcessDebugEvents;
                CurThread.RemoveNotifier(FNotifierIndex);
                FNotifierIndex := -1;
                if not FDeferredError then
                begin
                  if FDeferredResult <> '' then
                    Result := FDeferredResult
                  else
                    Result := ResultStr;
                end;
              end;
            erBusy:
              begin
                DebugSvcs.ProcessDebugEvents;
                Done := False;
              end;
          end;
        end
        until Done = True;
      end;
    end;
  end;
end;

procedure TStringListViewerFrame.EvaluateComplete(const ExprStr,
  ResultStr: string; CanModify: Boolean; ResultAddress, ResultSize: LongWord;
  ReturnCode: Integer);
begin
  EvaluateComplete(ExprStr, ResultStr, CanModify, TOTAAddress(ResultAddress), ResultSize, ReturnCode);
end;

procedure TStringListViewerFrame.EvaluateComplete(const ExprStr,
  ResultStr: string; CanModify: Boolean; ResultAddress: TOTAAddress; ResultSize: LongWord;
  ReturnCode: Integer);
begin
  FCompleted := True;
  FDeferredResult := ResultStr;
  FDeferredError := ReturnCode <> 0;
end;

function TStringListViewerFrame.GetDelimiter: string;
begin
  Result := Evaluate(Format('%s.Delimiter', [FExpression]));
end;

function TStringListViewerFrame.GetStrictDelimiter: Boolean;
begin
  Result := StrToBool(Evaluate(Format('%s.StrictDelimiter', [FExpression])));
end;

function TStringListViewerFrame.GetDelimitedText: string;
begin
  Result := Evaluate(Format('%s.DelimitedText', [FExpression]));
  Result := Copy(Result, 2, Length(Result) -2);
end;

function TStringListViewerFrame.GetText: string;
begin
  Result := Evaluate(Format('%s.Text', [FExpression]));
end;

procedure TStringListViewerFrame.GetSaveToFile(const AFileName: string);
begin
  Evaluate(Format('%s.SaveToFile(''%s'', TEncoding.UTF8)', [FExpression, AFileName]));
end;

procedure TStringListViewerFrame.MarkUnavailable(
  Reason: TOTAVisualizerUnavailableReason);
begin
  if Reason = ovurProcessRunning then
  begin
    FAvailableState := asProcRunning;
  end else if Reason = ovurOutOfScope then
    FAvailableState := asOutOfScope;
  StringListView.Items.Count := 1;
  StringListView.Invalidate;
  StringTextView.Clear;
end;

procedure TStringListViewerFrame.Modified;
begin

end;

procedure TStringListViewerFrame.ModifyComplete(const ExprStr,
  ResultStr: string; ReturnCode: Integer);
begin

end;

procedure TStringListViewerFrame.RefreshVisualizer(const Expression, TypeName,
  EvalResult: string);
begin
  FAvailableState := asAvailable;
  AddStringListItems(Expression, TypeName, EvalResult);
end;

procedure TStringListViewerFrame.SetClosedCallback(
  ClosedProc: TOTAVisualizerClosedProcedure);
begin
  FClosedProc := ClosedProc;
end;

procedure TStringListViewerFrame.SetForm(AForm: TCustomForm);
begin
  FOwningForm := AForm;
end;

procedure TStringListViewerFrame.SetParent(AParent: TWinControl);
begin
  if AParent = nil then
  begin
    FreeAndNil(FItems);
    if Assigned(FClosedProc) then
      FClosedProc;
  end;
  inherited;
end;

procedure TStringListViewerFrame.StringListViewData(Sender: TObject;
  Item: TListItem);
var
  ItemCaption: string;
  ItemText: string;
begin
  case FAvailableState of
    asAvailable:
      begin
        ItemCaption := Format('[%d]', [Item.Index]);
        ItemText := FItems[Item.Index];
      end;
    asProcRunning:
      begin
        ItemCaption := sProcessNotAccessible;
        ItemText := sProcessNotAccessible;
      end;
    asOutOfScope:
      begin
        ItemCaption := sOutOfScope;
        ItemText := sOutOfScope;
      end;
    asNotAvailable:
      begin
        ItemCaption := sValueNotAccessible;
        ItemText := sValueNotAccessible;
      end;
  end;
  Item.Caption := ItemCaption;
  if Item.SubItems.Count = 0 then
    Item.SubItems.Add(ItemText)
  else
    Item.SubItems[0] := ItemText;
end;

procedure TStringListViewerFrame.StringListViewDrawItem(Sender: TCustomListView;
  Item: TListItem; Rect: TRect; State: TOwnerDrawState);
const
  ListView_Padding = 8;
var
  LRect, LRect2: TRect;
  LText: string;
  LColor: TColor;
  LThemingServices: IOTAIDEThemingServices;
begin
  if Supports(BorlandIDEServices, IOTAIDEThemingServices, LThemingServices) and
    LThemingServices.IDEThemingEnabled then
  begin
    var LStyle := LThemingServices.StyleServices;
    Sender.Canvas.Brush.Style := bsSolid;
    Sender.Canvas.Brush.Color := LStyle.GetSystemColor(clWindow);
    Sender.Canvas.FillRect(Rect);

    LRect := Rect;
    for var I := 0 to TListView(Sender).Columns.Count - 1 do
    begin
      LRect.Right := LRect.Left + Sender.Column[i].Width;

      LText := '';
      if I = 0 then
        LText := Item.Caption
      else
      if (I - 1) <= Item.SubItems.Count - 1 then
        LText := Item.SubItems[I - 1];


      var LDetails := LStyle.GetElementDetails(tgCellNormal);
      LColor := LStyle.GetSystemColor(clWindowText);
      if ([odSelected, odHotLight] * State <> []) then
      begin
        LDetails := LStyle.GetElementDetails(tgCellSelected);
        LColor := LStyle.GetSystemColor(clHighlightText);
        LStyle.DrawElement(Sender.Canvas.Handle, LDetails, LRect);
      end;

      LRect2 := LRect;
      LRect2.Left := LRect2.Left + ListView_Padding;

      var LTextFormat := TTextFormatFlags(DT_SINGLELINE or DT_VCENTER or DT_LEFT or DT_END_ELLIPSIS);
      LStyle.DrawText(Sender.Canvas.Handle, LDetails, LText, LRect2, LTextFormat, LColor);

      Inc(LRect.Left, Sender.Column[i].Width);
    end;
  end;
end;


procedure TStringListViewerFrame.PCViewsChange(Sender: TObject);
begin
  if PCViews.ActivePage = TabList then
    StringListView.SetFocus
  else if PCViews.ActivePage = TabText then
    StringTextView.SetFocus;
end;

procedure TStringListViewerFrame.ThreadNotify(Reason: TOTANotifyReason);
begin

end;

{ TStringListVisualizerForm }

constructor TStringListVisualizerForm.Create(const Expression: string);
begin
  inherited Create;
  FExpression := Expression;
end;

procedure TStringListVisualizerForm.CustomizePopupMenu(PopupMenu: TPopupMenu);
begin
  // no toolbar
end;

procedure TStringListVisualizerForm.CustomizeToolBar(ToolBar: TToolBar);
begin
 // no toolbar
end;

function TStringListVisualizerForm.EditAction(Action: TEditAction): Boolean;
begin
  Result := False;
end;

procedure TStringListVisualizerForm.FrameCreated(AFrame: TCustomFrame);
begin
  FMyFrame :=  TStringListViewerFrame(AFrame);
end;

function TStringListVisualizerForm.GetCaption: string;
begin
  Result := Format(sFormCaption, [FExpression]);
end;

function TStringListVisualizerForm.GetEditState: TEditState;
begin
  Result := [];
end;

function TStringListVisualizerForm.GetForm: TCustomForm;
begin
  Result := FMyForm;
end;

function TStringListVisualizerForm.GetFrame: TCustomFrame;
begin
  Result := FMyFrame;
end;

function TStringListVisualizerForm.GetFrameClass: TCustomFrameClass;
begin
  Result := TStringListViewerFrame;
end;

function TStringListVisualizerForm.GetIdentifier: string;
begin
  Result := 'StringListDebugVisualizer';
end;

function TStringListVisualizerForm.GetMenuActionList: TCustomActionList;
begin
  Result := nil;
end;

function TStringListVisualizerForm.GetMenuImageList: TCustomImageList;
begin
  Result := nil;
end;

function TStringListVisualizerForm.GetToolbarActionList: TCustomActionList;
begin
  Result := nil;
end;

function TStringListVisualizerForm.GetToolbarImageList: TCustomImageList;
begin
  Result := nil;
end;

procedure TStringListVisualizerForm.LoadWindowState(Desktop: TCustomIniFile;
  const Section: string);
begin
  //no desktop saving
end;

procedure TStringListVisualizerForm.SaveWindowState(Desktop: TCustomIniFile;
  const Section: string; IsProject: Boolean);
begin
  //no desktop saving
end;

procedure TStringListVisualizerForm.SetForm(Form: TCustomForm);
begin
  FMyForm := Form;
  if Assigned(FMyFrame) then
    FMyFrame.SetForm(FMyForm);
end;

procedure TStringListVisualizerForm.SetFrame(Frame: TCustomFrame);
begin
   FMyFrame := TStringListViewerFrame(Frame);
end;

var
  StringListVis: IOTADebuggerVisualizer;

procedure Register;
begin
  StringListVis := TDebuggerStringListVisualizer.Create;
  (BorlandIDEServices as IOTADebuggerServices).RegisterDebugVisualizer(StringListVis);
end;

procedure RemoveVisualizer;
var
  DebuggerServices: IOTADebuggerServices;
begin
  if Supports(BorlandIDEServices, IOTADebuggerServices, DebuggerServices) then
  begin
    DebuggerServices.UnregisterDebugVisualizer(StringListVis);
    StringListVis := nil;
  end;
end;

initialization
finalization
  RemoveVisualizer;
end.

