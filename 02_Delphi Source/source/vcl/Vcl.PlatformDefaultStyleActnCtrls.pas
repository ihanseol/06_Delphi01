{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 1995-2024 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Vcl.PlatformDefaultStyleActnCtrls;

{$HPPEMIT LEGACYHPP}

interface

uses Vcl.ActnMan, Vcl.ActnMenus, Vcl.ActnCtrls;

type

{$IF NOT DEFINED(CLR)}
{$HPPEMIT ''}
{$HPPEMIT '/* automatically link to platformstyleactnctrls.obj so that the property editors are registered */'}
{$HPPEMIT LINKUNIT}
{$HPPEMIT ''}
{$ENDIF}

{ TPlatformDefaultStyleActionBars }

  TPlatformDefaultStyleActionBars = class(TActionBarStyleEx)
  public
    function GetColorMapClass(ActionBar: TCustomActionBar): TCustomColorMapClass; override;
    function GetControlClass(ActionBar: TCustomActionBar;
      AnItem: TActionClientItem): TCustomActionControlClass; override;
    function GetPopupClass(ActionBar: TCustomActionBar): TCustomPopupClass; override;
    function GetAddRemoveItemClass(ActionBar: TCustomActionBar): TCustomAddRemoveItemClass; override;
    function GetStyleName: string; override;
    function GetScrollBtnClass(ActionBar: TCustomActionBar): TCustomToolScrollBtnClass; override;
  end;

var
  PlatformDefaultStyle: TPlatformDefaultStyleActionBars;

implementation

uses
  Vcl.ListActns, Vcl.ActnColorMaps, System.SysUtils, Vcl.Themes, Vcl.XPActnCtrls,
  Vcl.StdActnMenus, Vcl.ThemedActnCtrls;

type
  TActionControlStyle = (csStandard, csXPStyle, csThemed);

function GetActionControlStyle(AActionBar: TCustomActionBar): TActionControlStyle;
var
  LStyle: TCustomStyleServices;
begin
  LStyle := AActionBar.StyleServices;
  if LStyle.Enabled then
    Result := csThemed
  else
  if TOSVersion.Check(5, 1) or TOSVersion.Check(6) then
    Result := csXPStyle
  else
    Result := csStandard;
end;

{ TPlatformDefaultStyleActionBars }

function TPlatformDefaultStyleActionBars.GetAddRemoveItemClass(
  ActionBar: TCustomActionBar): TCustomAddRemoveItemClass;
begin
  case GetActionControlStyle(ActionBar) of
    csStandard: Result := TStandardAddRemoveItem;
    csXPStyle: Result := TXPStyleAddRemoveItem;
  else
    Result := TThemedAddRemoveItem;
  end;
end;

function TPlatformDefaultStyleActionBars.GetColorMapClass(
  ActionBar: TCustomActionBar): TCustomColorMapClass;
begin
  case GetActionControlStyle(ActionBar) of
    csStandard: Result := TStandardColorMap;
    csXPStyle: Result := TXPColorMap;
  else
    Result := TThemedColorMap;
  end;
end;

function TPlatformDefaultStyleActionBars.GetControlClass(ActionBar: TCustomActionBar;
  AnItem: TActionClientItem): TCustomActionControlClass;
begin
  if ActionBar is TCustomActionToolBar then
  begin
    if AnItem.HasItems then
      case GetActionControlStyle(ActionBar) of
        csStandard: Result := TStandardDropDownButton;
        csXPStyle: Result := TXPStyleDropDownBtn;
      else
        Result := TThemedDropDownButton;
      end
    else
      if (AnItem.Action is TStaticListAction) or
         (AnItem.Action is TVirtualListAction) then
        Result := TCustomComboControl
      else
        case GetActionControlStyle(ActionBar) of
          csStandard: Result := TStandardButtonControl;
          csXPStyle: Result := TXPStyleButton;
        else
          Result := TThemedButtonControl;
        end
  end
  else if ActionBar is TCustomActionMainMenuBar then
    case GetActionControlStyle(ActionBar) of
      csStandard: Result := TStandardMenuButton;
      csXPStyle: Result := TXPStyleMenuButton;
    else
      Result := TThemedMenuButton;
    end
  else if ActionBar is TCustomizeActionToolBar then
  begin
    with TCustomizeActionToolbar(ActionBar) do
      if not Assigned(RootMenu) or
         (AnItem.ParentItem <> TCustomizeActionToolBar(RootMenu).AdditionalItem) then
        case GetActionControlStyle(ActionBar) of
          csStandard: Result := TStandardMenuItem;
          csXPStyle: Result := TXPStyleMenuItem;
        else
          Result := TThemedMenuItem;
        end
      else
        case GetActionControlStyle(ActionBar) of
          csStandard: Result := TStandardAddRemoveItem;
          csXPStyle: Result := TXPStyleAddRemoveItem;
        else
          Result := TThemedAddRemoveItem;
        end
  end
  else if ActionBar is TCustomActionPopupMenu then
    case GetActionControlStyle(ActionBar) of
      csStandard: Result := TStandardMenuItem;
      csXPStyle: Result := TXPStyleMenuItem;
    else
      Result := TThemedMenuItem;
    end
  else
    case GetActionControlStyle(ActionBar) of
      csStandard: Result := TStandardButtonControl;
      csXPStyle: Result := TXPStyleButton;
    else
      Result := TThemedButtonControl;
    end
end;

function TPlatformDefaultStyleActionBars.GetPopupClass(ActionBar: TCustomActionBar): TCustomPopupClass;
begin
  if ActionBar is TCustomActionToolBar then
    case GetActionControlStyle(ActionBar) of
      csStandard: Result := TStandardCustomizePopup;
      csXPStyle: Result := TXPStyleCustomizePopup;
    else
      Result := TThemedCustomizePopup;
    end
  else
    case GetActionControlStyle(ActionBar) of
      csStandard: Result := TStandardMenuPopup;
      csXPStyle: Result := TXPStylePopupMenu;
    else
      Result := TThemedPopupMenu;
    end;
end;

function TPlatformDefaultStyleActionBars.GetScrollBtnClass(ActionBar: TCustomActionBar): TCustomToolScrollBtnClass;
begin
  case GetActionControlStyle(ActionBar) of
    csStandard: Result := TStandardToolScrollBtn;
    csXPStyle: Result := TXPStyleToolScrollBtn;
  else
    Result := TThemedToolScrollBtn;
  end;
end;

function TPlatformDefaultStyleActionBars.GetStyleName: string;
begin
  Result := 'Platform Default'; { Do not localize }
end;

initialization
  PlatformDefaultStyle := TPlatformDefaultStyleActionBars.Create;
  DefaultActnBarStyle := PlatformDefaultStyle.GetStyleName;
  RegisterActnBarStyle(PlatformDefaultStyle);
finalization
  UnregisterActnBarStyle(PlatformDefaultStyle);
  PlatformDefaultStyle.Free;
end.

