{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{          File: oleacc.h                               }
{          Copyright (c) Microsoft Corporation          }
{                                                       }
{       Translator: Embarcadero Technologies, Inc.      }
{ Copyright(c) 1995-2024 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

{**********************************
 ***         IMPORTANT          ***
 **********************************

 This file has been modified from it's original because TLIBIMP did not
 import the file correctly.

 The command line used to import this file was as follows:

 tlibimp -Hs- -Hr- -Ftoleacc -Ps- -O- %systemroot%\SYSTEM32\OLEACC.DLL

 Additionally, there are many constants necessary for Accessibility that
 were not included in the TLB. These constants have been included here.

 }
unit Winapi.oleacc;

{$WEAKPACKAGEUNIT ON}

// NOTE: This file has been hand modified to remove units that were
//       included in the uses clause that otherwise would prevent this
//       unit from being included in the rtl package.

// ************************************************************************ //
// WARNING
// -------
// The types declared in this file were generated from data read from a
// Type Library. If this type library is explicitly or indirectly (via
// another type library referring to this type library) re-imported, or the
// 'Refresh' command of the Type Library Editor activated while editing the
// Type Library, the contents of this file will be regenerated and all
// manual modifications will be lost.
// ************************************************************************ //

// PASTLWTR : 1.2
// File generated on 3/20/2003 12:00:19 PM from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\WINDOWS\System32\oleacc.dll (1)
// LIBID: {1EA4DBF0-3C3B-11CF-810C-00AA00389B71}
// LCID: 0
// Helpfile:
// HelpString:
// DepndLst:
//   (1) v2.0 stdole, (C:\WINDOWS\System32\stdole2.tlb)
// Parent TypeLibrary:
//   (0) v1.0 Borland_Studio_ToolsAPI, (Borland.Studio.ToolsAPI.tlb)
// Errors:
//   Hint: Parameter 'var' of IAccPropServices.SetPropValue changed to 'var_'
//   Hint: Parameter 'var' of IAccPropServices.SetHwndProp changed to 'var_'
//   Hint: Parameter 'var' of IAccPropServices.SetHmenuProp changed to 'var_'
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers.
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
{$ALIGN ON}
{$MINENUMSIZE 4}
interface

uses Winapi.Windows, Winapi.ActiveX;

{$HPPEMIT '#include <oleacc.h>'}

{$HPPEMIT '#if !defined(_VCL_ALIAS_RECORDS)' }
{$HPPEMIT '    #pragma comment(lib, "oleacc")'}
{$HPPEMIT '#endif'}


// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:
//   Type Libraries     : LIBID_xxxx
//   CoClasses          : CLASS_xxxx
//   DISPInterfaces     : DIID_xxxx
//   Non-DISP interfaces: IID_xxxx
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  AccessibilityMajorVersion = 1;
  AccessibilityMinorVersion = 1;

  LIBID_Accessibility: TGUID = '{1EA4DBF0-3C3B-11CF-810C-00AA00389B71}';
  {$EXTERNALSYM LIBID_Accessibility}

  IID_IAccessible: TGUID = '{618736E0-3C3D-11CF-810C-00AA00389B71}';
  {$EXTERNALSYM IID_IAccessible}
  IID_IAccessibleHandler: TGUID = '{03022430-ABC4-11D0-BDE2-00AA001A1953}';
  {$EXTERNALSYM IID_IAccessibleHandler}
  IID_IAccIdentity: TGUID = '{7852B78D-1CFD-41C1-A615-9C0C85960B5F}';
  {$EXTERNALSYM IID_IAccIdentity}
  IID_IAccPropServer: TGUID = '{76C0DBBB-15E0-4E7B-B61B-20EEEA2001E0}';
  {$EXTERNALSYM IID_IAccPropServer}
  IID_IAccPropServices: TGUID = '{6E26E776-04F0-495D-80E4-3330352E3169}';
  {$EXTERNALSYM IID_IAccPropServices}
  IID_IAccPropMgrInternal: TGUID = '{2BD370A9-3E7F-4EDD-8A85-F8FED1F8E51F}';
  {$EXTERNALSYM IID_IAccPropMgrInternal}
  CLSID_AccPropServices: TGUID = '{B5F8350B-0548-48B1-A6EE-88BD00B4A5E7}';
  {$EXTERNALSYM CLSID_AccPropServices}
  CLSID_CAccPropServices: TGUID = '{B5F8350B-0548-48B1-A6EE-88BD00B4A5E7}';
  {$EXTERNALSYM CLSID_CAccPropServices}
  IIS_IsOleaccProxy: TGUID = '{902697FA-80E4-4560-802A-A13F22A64709}';
  {$EXTERNALSYM IIS_IsOleaccProxy}
  IIS_ControlAccessible: TGUID = '{38C682A6-9731-43F2-9FAE-E901E641B101}';
  {$EXTERNALSYM IIS_ControlAccessible}

  PROPID_ACC_NAME: TGUID = '{608D3DF8-8128-4AA7-A428-F55E49267291}';
  {$EXTERNALSYM PROPID_ACC_NAME}
  PROPID_ACC_VALUE: TGUID = '{123FE443-211A-4615-9527-C45A7E93717A}';
  {$EXTERNALSYM PROPID_ACC_VALUE}
  PROPID_ACC_DESCRIPTION: TGUID = '{4D48DFE4-BD3F-491F-A648-492D6F20C588}';
  {$EXTERNALSYM PROPID_ACC_DESCRIPTION}
  PROPID_ACC_ROLE: TGUID = '{CB905FF2-7BD1-4C05-B3C8-E6C241364D70}';
  {$EXTERNALSYM PROPID_ACC_ROLE}
  PROPID_ACC_STATE: TGUID = '{A8D4D5B0-0A21-42D0-A5C0-514E984F457B}';
  {$EXTERNALSYM PROPID_ACC_STATE}
  PROPID_ACC_HELP: TGUID = '{C831E11F-44DB-4A99-9768-CB8F978B7231}';
  {$EXTERNALSYM PROPID_ACC_HELP}
  PROPID_ACC_KEYBOARDSHORTCUT: TGUID = '{7D9BCEEE-7D1E-4979-9382-5180F4172C34}';
  {$EXTERNALSYM PROPID_ACC_KEYBOARDSHORTCUT}
  PROPID_ACC_DEFAULTACTION: TGUID = '{180C072B-C27F-43C7-9922-F63562A4632B}';
  {$EXTERNALSYM PROPID_ACC_DEFAULTACTION}

  PROPID_ACC_HELPTOPIC: TGUID = '{787D1379-8EDE-440B-8AEC-11F7BF9030B3}';
  {$EXTERNALSYM PROPID_ACC_HELPTOPIC}
  PROPID_ACC_FOCUS: TGUID = '{6EB335DF-1C29-4127-B12C-DEE9FD157F2B}';
  {$EXTERNALSYM PROPID_ACC_FOCUS}
  PROPID_ACC_SELECTION: TGUID = '{B99D073C-D731-405B-9061-D95E8F842984}';
  {$EXTERNALSYM PROPID_ACC_SELECTION}
  PROPID_ACC_PARENT: TGUID = '{474C22B6-FFC2-467A-B1B5-E958B4657330}';
  {$EXTERNALSYM PROPID_ACC_PARENT}

  PROPID_ACC_NAV_UP: TGUID = '{016E1A2B-1A4E-4767-8612-3386F66935EC}';
  {$EXTERNALSYM PROPID_ACC_NAV_UP}
  PROPID_ACC_NAV_DOWN: TGUID = '{031670ED-3CDF-48D2-9613-138F2DD8A668}';
  {$EXTERNALSYM PROPID_ACC_NAV_DOWN}
  PROPID_ACC_NAV_LEFT: TGUID = '{228086CB-82F1-4A39-8705-DCDC0FFF92F5}';
  {$EXTERNALSYM PROPID_ACC_NAV_LEFT}
  PROPID_ACC_NAV_RIGHT: TGUID = '{CD211D9F-E1CB-4FE5-A77C-920B884D095B}';
  {$EXTERNALSYM PROPID_ACC_NAV_RIGHT}
  PROPID_ACC_NAV_PREV: TGUID = '{776D3891-C73B-4480-B3F6-076A16A15AF6}';
  {$EXTERNALSYM PROPID_ACC_NAV_PREV}
  PROPID_ACC_NAV_NEXT: TGUID = '{1CDC5455-8CD9-4C92-A371-3939A2FE3EEE}';
  {$EXTERNALSYM PROPID_ACC_NAV_NEXT}
  PROPID_ACC_NAV_FIRSTCHILD: TGUID = '{CFD02558-557B-4C67-84F9-2A09FCE40749}';
  {$EXTERNALSYM PROPID_ACC_NAV_FIRSTCHILD}
  PROPID_ACC_NAV_LASTCHILD: TGUID = '{302ECAA5-48D5-4F8D-B671-1A8D20A77832}';
  {$EXTERNALSYM PROPID_ACC_NAV_LASTCHILD}

  // Value map, used by sliders and other controls...
  PROPID_ACC_ROLEMAP: TGUID = '{F79ACDA2-140D-4FE6-8914-208476328269}';
  {$EXTERNALSYM PROPID_ACC_ROLEMAP}
  PROPID_ACC_VALUEMAP: TGUID = '{DA1C3D79-FC5C-420E-B399-9D1533549E75}';
  {$EXTERNALSYM PROPID_ACC_VALUEMAP}
  PROPID_ACC_STATEMAP: TGUID = '{43946C5E-0AC0-4042-B525-07BBDBE17FA7}';
  {$EXTERNALSYM PROPID_ACC_STATEMAP}
  PROPID_ACC_DESCRIPTIONMAP: TGUID = '{1FF1435F-8A14-477B-B226-A0ABE279975D}';
  {$EXTERNALSYM PROPID_ACC_DESCRIPTIONMAP}

  PROPID_ACC_DODEFAULTACTION: TGUID = '{1BA09523-2E3B-49A6-A059-59682A3C48FD}';
  {$EXTERNALSYM PROPID_ACC_DODEFAULTACTION}

// *********************************************************************//
// Declaration of Enumerations defined in Type Library
// *********************************************************************//
// Constants for enum AnnoScope
type
  AnnoScope = TOleEnum;
  {$EXTERNALSYM AnnoScope}
const
  ANNO_THIS = $00000000;
  {$EXTERNALSYM ANNO_THIS}
  ANNO_CONTAINER = $00000001;
  {$EXTERNALSYM ANNO_CONTAINER}

// BEGIN: Constants added from oleacc.h

  DISPID_ACC_PARENT = -5000 ;
  {$EXTERNALSYM DISPID_ACC_PARENT}
  DISPID_ACC_CHILDCOUNT = -5001 ;
  {$EXTERNALSYM DISPID_ACC_CHILDCOUNT}
  DISPID_ACC_CHILD = -5002 ;
  {$EXTERNALSYM DISPID_ACC_CHILD}
  DISPID_ACC_NAME = -5003 ;
  {$EXTERNALSYM DISPID_ACC_NAME}
  DISPID_ACC_VALUE = -5004 ;
  {$EXTERNALSYM DISPID_ACC_VALUE}
  DISPID_ACC_DESCRIPTION = -5005 ;
  {$EXTERNALSYM DISPID_ACC_DESCRIPTION}
  DISPID_ACC_ROLE = -5006 ;
  {$EXTERNALSYM DISPID_ACC_ROLE}
  DISPID_ACC_STATE = -5007 ;
  {$EXTERNALSYM DISPID_ACC_STATE}
  DISPID_ACC_HELP = -5008 ;
  {$EXTERNALSYM DISPID_ACC_HELP}
  DISPID_ACC_HELPTOPIC = -5009 ;
  {$EXTERNALSYM DISPID_ACC_HELPTOPIC}
  DISPID_ACC_KEYBOARDSHORTCUT = -5010 ;
  {$EXTERNALSYM DISPID_ACC_KEYBOARDSHORTCUT}
  DISPID_ACC_FOCUS = -5011 ;
  {$EXTERNALSYM DISPID_ACC_FOCUS}
  DISPID_ACC_SELECTION = -5012 ;
  {$EXTERNALSYM DISPID_ACC_SELECTION}
  DISPID_ACC_DEFAULTACTION = -5013 ;
  {$EXTERNALSYM DISPID_ACC_DEFAULTACTION}
  DISPID_ACC_SELECT = -5014 ;
  {$EXTERNALSYM DISPID_ACC_SELECT}
  DISPID_ACC_LOCATION = -5015 ;
  {$EXTERNALSYM DISPID_ACC_LOCATION}
  DISPID_ACC_NAVIGATE = -5016 ;
  {$EXTERNALSYM DISPID_ACC_NAVIGATE}
  DISPID_ACC_HITTEST = -5017 ;
  {$EXTERNALSYM DISPID_ACC_HITTEST}
  DISPID_ACC_DODEFAULTACTION = -5018 ;
  {$EXTERNALSYM DISPID_ACC_DODEFAULTACTION}
  NAVDIR_MIN = 0 ;
  {$EXTERNALSYM NAVDIR_MIN}
  NAVDIR_UP = $1 ;
  {$EXTERNALSYM NAVDIR_UP}
  NAVDIR_DOWN = $2 ;
  {$EXTERNALSYM NAVDIR_DOWN}
  NAVDIR_LEFT = $3 ;
  {$EXTERNALSYM NAVDIR_LEFT}
  NAVDIR_RIGHT = $4 ;
  {$EXTERNALSYM NAVDIR_RIGHT}
  NAVDIR_NEXT = $5 ;
  {$EXTERNALSYM NAVDIR_NEXT}
  NAVDIR_PREVIOUS = $6 ;
  {$EXTERNALSYM NAVDIR_PREVIOUS}
  NAVDIR_FIRSTCHILD = $7 ;
  {$EXTERNALSYM NAVDIR_FIRSTCHILD}
  NAVDIR_LASTCHILD = $8 ;
  {$EXTERNALSYM NAVDIR_LASTCHILD}
  NAVDIR_MAX = $9 ;
  {$EXTERNALSYM NAVDIR_MAX}
  SELFLAG_NONE = 0 ;
  {$EXTERNALSYM SELFLAG_NONE}
  SELFLAG_TAKEFOCUS = $1 ;
  {$EXTERNALSYM SELFLAG_TAKEFOCUS}
  SELFLAG_TAKESELECTION = $2 ;
  {$EXTERNALSYM SELFLAG_TAKESELECTION}
  SELFLAG_EXTENDSELECTION = $4 ;
  {$EXTERNALSYM SELFLAG_EXTENDSELECTION}
  SELFLAG_ADDSELECTION = $8 ;
  {$EXTERNALSYM SELFLAG_ADDSELECTION}
  SELFLAG_REMOVESELECTION = $10 ;
  {$EXTERNALSYM SELFLAG_REMOVESELECTION}
  SELFLAG_VALID = $1f ;
  {$EXTERNALSYM SELFLAG_VALID}
  STATE_SYSTEM_NORMAL = 0 ;
  {$EXTERNALSYM STATE_SYSTEM_NORMAL}
  STATE_SYSTEM_UNAVAILABLE = $1 ;
  {$EXTERNALSYM STATE_SYSTEM_UNAVAILABLE}
  STATE_SYSTEM_SELECTED = $2 ;
  {$EXTERNALSYM STATE_SYSTEM_SELECTED}
  STATE_SYSTEM_FOCUSED = $4 ;
  {$EXTERNALSYM STATE_SYSTEM_FOCUSED}
  STATE_SYSTEM_PRESSED = $8 ;
  {$EXTERNALSYM STATE_SYSTEM_PRESSED}
  STATE_SYSTEM_CHECKED = $10 ;
  {$EXTERNALSYM STATE_SYSTEM_CHECKED}
  STATE_SYSTEM_MIXED = $20 ;
  {$EXTERNALSYM STATE_SYSTEM_MIXED}
  STATE_SYSTEM_INDETERMINATE = STATE_SYSTEM_MIXED;
  {$EXTERNALSYM STATE_SYSTEM_INDETERMINATE}
  STATE_SYSTEM_READONLY = $40 ;
  {$EXTERNALSYM STATE_SYSTEM_READONLY}
  STATE_SYSTEM_HOTTRACKED = $80 ;
  {$EXTERNALSYM STATE_SYSTEM_HOTTRACKED}
  STATE_SYSTEM_DEFAULT = $100 ;
  {$EXTERNALSYM STATE_SYSTEM_DEFAULT}
  STATE_SYSTEM_EXPANDED = $200 ;
  {$EXTERNALSYM STATE_SYSTEM_EXPANDED}
  STATE_SYSTEM_COLLAPSED = $400 ;
  {$EXTERNALSYM STATE_SYSTEM_COLLAPSED}
  STATE_SYSTEM_BUSY = $800 ;
  {$EXTERNALSYM STATE_SYSTEM_BUSY}
  STATE_SYSTEM_FLOATING = $1000 ;
  {$EXTERNALSYM STATE_SYSTEM_FLOATING}
  STATE_SYSTEM_MARQUEED = $2000 ;
  {$EXTERNALSYM STATE_SYSTEM_MARQUEED}
  STATE_SYSTEM_ANIMATED = $4000 ;
  {$EXTERNALSYM STATE_SYSTEM_ANIMATED}
  STATE_SYSTEM_INVISIBLE = $8000 ;
  {$EXTERNALSYM STATE_SYSTEM_INVISIBLE}
  STATE_SYSTEM_OFFSCREEN = $10000 ;
  {$EXTERNALSYM STATE_SYSTEM_OFFSCREEN}
  STATE_SYSTEM_SIZEABLE = $20000 ;
  {$EXTERNALSYM STATE_SYSTEM_SIZEABLE}
  STATE_SYSTEM_MOVEABLE = $40000 ;
  {$EXTERNALSYM STATE_SYSTEM_MOVEABLE}
  STATE_SYSTEM_SELFVOICING = $80000 ;
  {$EXTERNALSYM STATE_SYSTEM_SELFVOICING}
  STATE_SYSTEM_FOCUSABLE = $100000 ;
  {$EXTERNALSYM STATE_SYSTEM_FOCUSABLE}
  STATE_SYSTEM_SELECTABLE = $200000 ;
  {$EXTERNALSYM STATE_SYSTEM_SELECTABLE}
  STATE_SYSTEM_LINKED = $400000 ;
  {$EXTERNALSYM STATE_SYSTEM_LINKED}
  STATE_SYSTEM_TRAVERSED = $800000 ;
  {$EXTERNALSYM STATE_SYSTEM_TRAVERSED}
  STATE_SYSTEM_MULTISELECTABLE = $1000000 ;
  {$EXTERNALSYM STATE_SYSTEM_MULTISELECTABLE}
  STATE_SYSTEM_EXTSELECTABLE = $2000000 ;
  {$EXTERNALSYM STATE_SYSTEM_EXTSELECTABLE}
  STATE_SYSTEM_ALERT_LOW = $4000000 ;
  {$EXTERNALSYM STATE_SYSTEM_ALERT_LOW}
  STATE_SYSTEM_ALERT_MEDIUM = $8000000 ;
  {$EXTERNALSYM STATE_SYSTEM_ALERT_MEDIUM}
  STATE_SYSTEM_ALERT_HIGH = $10000000 ;
  {$EXTERNALSYM STATE_SYSTEM_ALERT_HIGH}
  STATE_SYSTEM_PROTECTED = $20000000 ;
  {$EXTERNALSYM STATE_SYSTEM_PROTECTED}
  STATE_SYSTEM_HASPOPUP = $40000000;
  {$EXTERNALSYM STATE_SYSTEM_HASPOPUP}
  STATE_SYSTEM_VALID = $3fffffff ;
  {$EXTERNALSYM STATE_SYSTEM_VALID}
  ROLE_SYSTEM_TITLEBAR = $1 ;
  {$EXTERNALSYM ROLE_SYSTEM_TITLEBAR}
  ROLE_SYSTEM_MENUBAR = $2 ;
  {$EXTERNALSYM ROLE_SYSTEM_MENUBAR}
  ROLE_SYSTEM_SCROLLBAR = $3 ;
  {$EXTERNALSYM ROLE_SYSTEM_SCROLLBAR}
  ROLE_SYSTEM_GRIP = $4 ;
  {$EXTERNALSYM ROLE_SYSTEM_GRIP}
  ROLE_SYSTEM_SOUND = $5 ;
  {$EXTERNALSYM ROLE_SYSTEM_SOUND}
  ROLE_SYSTEM_CURSOR = $6 ;
  {$EXTERNALSYM ROLE_SYSTEM_CURSOR}
  ROLE_SYSTEM_CARET = $7 ;
  {$EXTERNALSYM ROLE_SYSTEM_CARET}
  ROLE_SYSTEM_ALERT = $8 ;
  {$EXTERNALSYM ROLE_SYSTEM_ALERT}
  ROLE_SYSTEM_WINDOW = $9 ;
  {$EXTERNALSYM ROLE_SYSTEM_WINDOW}
  ROLE_SYSTEM_CLIENT = $a ;
  {$EXTERNALSYM ROLE_SYSTEM_CLIENT}
  ROLE_SYSTEM_MENUPOPUP = $b ;
  {$EXTERNALSYM ROLE_SYSTEM_MENUPOPUP}
  ROLE_SYSTEM_MENUITEM = $c ;
  {$EXTERNALSYM ROLE_SYSTEM_MENUITEM}
  ROLE_SYSTEM_TOOLTIP = $d ;
  {$EXTERNALSYM ROLE_SYSTEM_TOOLTIP}
  ROLE_SYSTEM_APPLICATION = $e ;
  {$EXTERNALSYM ROLE_SYSTEM_APPLICATION}
  ROLE_SYSTEM_DOCUMENT = $f ;
  {$EXTERNALSYM ROLE_SYSTEM_DOCUMENT}
  ROLE_SYSTEM_PANE = $10 ;
  {$EXTERNALSYM ROLE_SYSTEM_PANE}
  ROLE_SYSTEM_CHART = $11 ;
  {$EXTERNALSYM ROLE_SYSTEM_CHART}
  ROLE_SYSTEM_DIALOG = $12 ;
  {$EXTERNALSYM ROLE_SYSTEM_DIALOG}
  ROLE_SYSTEM_BORDER = $13 ;
  {$EXTERNALSYM ROLE_SYSTEM_BORDER}
  ROLE_SYSTEM_GROUPING = $14 ;
  {$EXTERNALSYM ROLE_SYSTEM_GROUPING}
  ROLE_SYSTEM_SEPARATOR = $15 ;
  {$EXTERNALSYM ROLE_SYSTEM_SEPARATOR}
  ROLE_SYSTEM_TOOLBAR = $16 ;
  {$EXTERNALSYM ROLE_SYSTEM_TOOLBAR}
  ROLE_SYSTEM_STATUSBAR = $17 ;
  {$EXTERNALSYM ROLE_SYSTEM_STATUSBAR}
  ROLE_SYSTEM_TABLE = $18 ;
  {$EXTERNALSYM ROLE_SYSTEM_TABLE}
  ROLE_SYSTEM_COLUMNHEADER = $19 ;
  {$EXTERNALSYM ROLE_SYSTEM_COLUMNHEADER}
  ROLE_SYSTEM_ROWHEADER = $1a ;
  {$EXTERNALSYM ROLE_SYSTEM_ROWHEADER}
  ROLE_SYSTEM_COLUMN = $1b ;
  {$EXTERNALSYM ROLE_SYSTEM_COLUMN}
  ROLE_SYSTEM_ROW = $1c ;
  {$EXTERNALSYM ROLE_SYSTEM_ROW}
  ROLE_SYSTEM_CELL = $1d ;
  {$EXTERNALSYM ROLE_SYSTEM_CELL}
  ROLE_SYSTEM_LINK = $1e ;
  {$EXTERNALSYM ROLE_SYSTEM_LINK}
  ROLE_SYSTEM_HELPBALLOON = $1f ;
  {$EXTERNALSYM ROLE_SYSTEM_HELPBALLOON}
  ROLE_SYSTEM_CHARACTER = $20 ;
  {$EXTERNALSYM ROLE_SYSTEM_CHARACTER}
  ROLE_SYSTEM_LIST = $21 ;
  {$EXTERNALSYM ROLE_SYSTEM_LIST}
  ROLE_SYSTEM_LISTITEM = $22 ;
  {$EXTERNALSYM ROLE_SYSTEM_LISTITEM}
  ROLE_SYSTEM_OUTLINE = $23 ;
  {$EXTERNALSYM ROLE_SYSTEM_OUTLINE}
  ROLE_SYSTEM_OUTLINEITEM = $24 ;
  {$EXTERNALSYM ROLE_SYSTEM_OUTLINEITEM}
  ROLE_SYSTEM_PAGETAB = $25 ;
  {$EXTERNALSYM ROLE_SYSTEM_PAGETAB}
  ROLE_SYSTEM_PROPERTYPAGE = $26 ;
  {$EXTERNALSYM ROLE_SYSTEM_PROPERTYPAGE}
  ROLE_SYSTEM_INDICATOR = $27 ;
  {$EXTERNALSYM ROLE_SYSTEM_INDICATOR}
  ROLE_SYSTEM_GRAPHIC = $28 ;
  {$EXTERNALSYM ROLE_SYSTEM_GRAPHIC}
  ROLE_SYSTEM_STATICTEXT = $29 ;
  {$EXTERNALSYM ROLE_SYSTEM_STATICTEXT}
  ROLE_SYSTEM_TEXT = $2a ;
  {$EXTERNALSYM ROLE_SYSTEM_TEXT}
  ROLE_SYSTEM_PUSHBUTTON = $2b ;
  {$EXTERNALSYM ROLE_SYSTEM_PUSHBUTTON}
  ROLE_SYSTEM_CHECKBUTTON = $2c ;
  {$EXTERNALSYM ROLE_SYSTEM_CHECKBUTTON}
  ROLE_SYSTEM_RADIOBUTTON = $2d ;
  {$EXTERNALSYM ROLE_SYSTEM_RADIOBUTTON}
  ROLE_SYSTEM_COMBOBOX = $2e ;
  {$EXTERNALSYM ROLE_SYSTEM_COMBOBOX}
  ROLE_SYSTEM_DROPLIST = $2f ;
  {$EXTERNALSYM ROLE_SYSTEM_DROPLIST}
  ROLE_SYSTEM_PROGRESSBAR = $30 ;
  {$EXTERNALSYM ROLE_SYSTEM_PROGRESSBAR}
  ROLE_SYSTEM_DIAL = $31 ;
  {$EXTERNALSYM ROLE_SYSTEM_DIAL}
  ROLE_SYSTEM_HOTKEYFIELD = $32 ;
  {$EXTERNALSYM ROLE_SYSTEM_HOTKEYFIELD}
  ROLE_SYSTEM_SLIDER = $33 ;
  {$EXTERNALSYM ROLE_SYSTEM_SLIDER}
  ROLE_SYSTEM_SPINBUTTON = $34 ;
  {$EXTERNALSYM ROLE_SYSTEM_SPINBUTTON}
  ROLE_SYSTEM_DIAGRAM = $35 ;
  {$EXTERNALSYM ROLE_SYSTEM_DIAGRAM}
  ROLE_SYSTEM_ANIMATION = $36 ;
  {$EXTERNALSYM ROLE_SYSTEM_ANIMATION}
  ROLE_SYSTEM_EQUATION = $37 ;
  {$EXTERNALSYM ROLE_SYSTEM_EQUATION}
  ROLE_SYSTEM_BUTTONDROPDOWN = $38 ;
  {$EXTERNALSYM ROLE_SYSTEM_BUTTONDROPDOWN}
  ROLE_SYSTEM_BUTTONMENU = $39 ;
  {$EXTERNALSYM ROLE_SYSTEM_BUTTONMENU}
  ROLE_SYSTEM_BUTTONDROPDOWNGRID = $3a ;
  {$EXTERNALSYM ROLE_SYSTEM_BUTTONDROPDOWNGRID}
  ROLE_SYSTEM_WHITESPACE = $3b ;
  {$EXTERNALSYM ROLE_SYSTEM_WHITESPACE}
  ROLE_SYSTEM_PAGETABLIST = $3c ;
  {$EXTERNALSYM ROLE_SYSTEM_PAGETABLIST}
  ROLE_SYSTEM_CLOCK = $3d ;
  {$EXTERNALSYM ROLE_SYSTEM_CLOCK}
  ROLE_SYSTEM_SPLITBUTTON = $3e ;
  {$EXTERNALSYM ROLE_SYSTEM_SPLITBUTTON}
  ROLE_SYSTEM_IPADDRESS = $3f ;
  {$EXTERNALSYM ROLE_SYSTEM_IPADDRESS}
  ROLE_SYSTEM_OUTLINEBUTTON = $40 ;
  {$EXTERNALSYM ROLE_SYSTEM_OUTLINEBUTTON}
  CHILDID_SELF = 0;
  {$EXTERNALSYM CHILDID_SELF}

const
  IID_IAccessibleWindowlessSite_Name = '{BF3ABD9C-76DA-4389-9EB6-1427D25ABAB7}';
  IID_IAccessibleWindowlessSite: TGuid = IID_IAccessibleWindowlessSite_Name;
  {$EXTERNALSYM IID_IAccessibleWindowlessSite}

// END: Constants added from oleacc.h

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary
// *********************************************************************//
  {$EXTERNALSYM IAccessible}
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAccessible)' }
  IAccessible = interface;
  {$EXTERNALSYM IAccessibleDisp}
  IAccessibleDisp = dispinterface;
  {$EXTERNALSYM IAccessibleHandler}
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAccessibleHandler)' }
  IAccessibleHandler = interface;
  {$EXTERNALSYM IAccIdentity}
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAccIdentity)' }
  IAccIdentity = interface;
  {$EXTERNALSYM IAccPropServer}
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAccPropServer)' }
  IAccPropServer = interface;
  {$EXTERNALSYM IAccPropServices}
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAccPropServices)' }
  IAccPropServices = interface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library
// (NOTE: Here we map each CoClass to its Default Interface)
// *********************************************************************//
  {$EXTERNALSYM CAccPropServices}
  CAccPropServices = IAccPropServices;


// *********************************************************************//
// Declaration of structures, unions and aliases.
// *********************************************************************//
  {$EXTERNALSYM wireHWND}
  wireHWND = ^_RemotableHandle;
  {$EXTERNALSYM wireHMENU}
  wireHMENU = ^_RemotableHandle;
  PByte1 = ^Byte; {*}
  PUserType1 = ^TGUID; {*}


  __MIDL_IWinTypes_0009 = record
    case Integer of
      0: (hInproc: Integer);
      1: (hRemote: Integer);
  end;
  {$EXTERNALSYM __MIDL_IWinTypes_0009}

  _RemotableHandle = record
    fContext: Integer;
    u: __MIDL_IWinTypes_0009;
  end;
  {$EXTERNALSYM _RemotableHandle}


// *********************************************************************//
// Interface: IAccessible
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {618736E0-3C3D-11CF-810C-00AA00389B71}
// *********************************************************************//
  {$EXTERNALSYM IAccessible}
  IAccessible = interface(IDispatch)
    ['{618736E0-3C3D-11CF-810C-00AA00389B71}']
    function Get_accParent(out ppdispParent: IDispatch): HResult; stdcall;
    function Get_accChildCount(out pcountChildren: Integer): HResult; stdcall;
    function Get_accChild(varChild: OleVariant; out ppdispChild: IDispatch): HResult; stdcall;
    function Get_accName(varChild: OleVariant; out pszName: WideString): HResult; stdcall;
    function Get_accValue(varChild: OleVariant; out pszValue: WideString): HResult; stdcall;
    function Get_accDescription(varChild: OleVariant; out pszDescription: WideString): HResult; stdcall;
    function Get_accRole(varChild: OleVariant; out pvarRole: OleVariant): HResult; stdcall;
    function Get_accState(varChild: OleVariant; out pvarState: OleVariant): HResult; stdcall;
    function Get_accHelp(varChild: OleVariant; out pszHelp: WideString): HResult; stdcall;
    function Get_accHelpTopic(out pszHelpFile: WideString; varChild: OleVariant;
                              out pidTopic: Integer): HResult; stdcall;
    function Get_accKeyboardShortcut(varChild: OleVariant; out pszKeyboardShortcut: WideString): HResult; stdcall;
    function Get_accFocus(out pvarChild: OleVariant): HResult; stdcall;
    function Get_accSelection(out pvarChildren: OleVariant): HResult; stdcall;
    function Get_accDefaultAction(varChild: OleVariant; out pszDefaultAction: WideString): HResult; stdcall;
    function accSelect(flagsSelect: Integer; varChild: OleVariant): HResult; stdcall;
    function accLocation(out pxLeft: Integer; out pyTop: Integer; out pcxWidth: Integer;
                         out pcyHeight: Integer; varChild: OleVariant): HResult; stdcall;
    function accNavigate(navDir: Integer; varStart: OleVariant; out pvarEndUpAt: OleVariant): HResult; stdcall;
    function accHitTest(xLeft: Integer; yTop: Integer; out pvarChild: OleVariant): HResult; stdcall;
    function accDoDefaultAction(varChild: OleVariant): HResult; stdcall;
    function Set_accName(varChild: OleVariant; const pszName: WideString): HResult; stdcall;
    function Set_accValue(varChild: OleVariant; const pszValue: WideString): HResult; stdcall;
  end;

// *********************************************************************//
// DispIntf:  IAccessibleDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {618736E0-3C3D-11CF-810C-00AA00389B71}
// *********************************************************************//
  {$EXTERNALSYM IAccessibleDisp}
  IAccessibleDisp = dispinterface
    ['{618736E0-3C3D-11CF-810C-00AA00389B71}']
    property accParent: IDispatch readonly dispid -5000;
    property accChildCount: Integer readonly dispid -5001;
    property accChild[varChild: OleVariant]: IDispatch readonly dispid -5002;
    property accName[varChild: OleVariant]: WideString dispid -5003;
    property accValue[varChild: OleVariant]: WideString dispid -5004;
    property accDescription[varChild: OleVariant]: WideString readonly dispid -5005;
    property accRole[varChild: OleVariant]: OleVariant readonly dispid -5006;
    property accState[varChild: OleVariant]: OleVariant readonly dispid -5007;
    property accHelp[varChild: OleVariant]: WideString readonly dispid -5008;
    property accHelpTopic[out pszHelpFile: WideString; varChild: OleVariant]: Integer readonly dispid -5009;
    property accKeyboardShortcut[varChild: OleVariant]: WideString readonly dispid -5010;
    property accFocus: OleVariant readonly dispid -5011;
    property accSelection: OleVariant readonly dispid -5012;
    property accDefaultAction[varChild: OleVariant]: WideString readonly dispid -5013;
    procedure accSelect(flagsSelect: Integer; varChild: OleVariant); dispid -5014;
    procedure accLocation(out pxLeft: Integer; out pyTop: Integer; out pcxWidth: Integer;
                          out pcyHeight: Integer; varChild: OleVariant); dispid -5015;
    function accNavigate(navDir: Integer; varStart: OleVariant): OleVariant; dispid -5016;
    function accHitTest(xLeft: Integer; yTop: Integer): OleVariant; dispid -5017;
    procedure accDoDefaultAction(varChild: OleVariant); dispid -5018;
  end;

// *********************************************************************//
// Interface: IAccessibleHandler
// Flags:     (272) Hidden OleAutomation
// GUID:      {03022430-ABC4-11D0-BDE2-00AA001A1953}
// *********************************************************************//
  {$EXTERNALSYM IAccessibleHandler}
  IAccessibleHandler = interface(IUnknown)
    ['{03022430-ABC4-11D0-BDE2-00AA001A1953}']
    function AccessibleObjectFromID(hwnd: Integer; lObjectID: Integer; out pIAccessible: IAccessible): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IAccessibleWindowlessSite
// GUID:      {BF3ABD9C-76DA-4389-9EB6-1427D25ABAB7}
// *********************************************************************//
  {$EXTERNALSYM IAccessibleWindowlessSite}
  IAccessibleWindowlessSite = interface(IUnknown)
    [IID_IAccessibleWindowlessSite_Name]
    function AcquireObjectIdRange(rangeSize: Integer; const rangeOwner: IAccessibleHandler;
	  out rangeBase: Integer): HResult; stdcall;
    function ReleaseObjectIdRange(rangeBase: Integer; const rangeOwner: IAccessibleHandler): HResult; stdcall;
    function QueryObjectIdRanges(const rangesOwner: IAccessibleHandler; out psaRanges: PSAFEARRAY): HResult; stdcall;
    function GetParentAccessible(out parent: IAccessible): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IAccIdentity
// Flags:     (0)
// GUID:      {7852B78D-1CFD-41C1-A615-9C0C85960B5F}
// *********************************************************************//
  {$EXTERNALSYM IAccIdentity}
  IAccIdentity = interface(IUnknown)
    ['{7852B78D-1CFD-41C1-A615-9C0C85960B5F}']
    function GetIdentityString(dwIDChild: LongWord; out ppIDString: PByte1;
                               out pdwIDStringLen: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IAccPropServer
// Flags:     (0)
// GUID:      {76C0DBBB-15E0-4E7B-B61B-20EEEA2001E0}
// *********************************************************************//
  {$EXTERNALSYM IAccPropServer}
  IAccPropServer = interface(IUnknown)
    ['{76C0DBBB-15E0-4E7B-B61B-20EEEA2001E0}']
    function GetPropValue(var pIDString: Byte; dwIDStringLen: LongWord; idProp: TGUID;
                          out pvarValue: OleVariant; out pfHasProp: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IAccPropServices
// Flags:     (0)
// GUID:      {6E26E776-04F0-495D-80E4-3330352E3169}
// *********************************************************************//
  {$EXTERNALSYM IAccPropServices}
  IAccPropServices = interface(IUnknown)
    ['{6E26E776-04F0-495D-80E4-3330352E3169}']
    function SetPropValue(var pIDString: Byte; dwIDStringLen: LongWord; idProp: TGUID;
                          var_: OleVariant): HResult; stdcall;
    function SetPropServer(var pIDString: Byte; dwIDStringLen: LongWord; var paProps: TGUID;
                           cProps: SYSINT; const pServer: IAccPropServer; AnnoScope: AnnoScope): HResult; stdcall;
    function ClearProps(var pIDString: Byte; dwIDStringLen: LongWord; var paProps: TGUID;
                        cProps: SYSINT): HResult; stdcall;
    function SetHwndProp(var hwnd: _RemotableHandle; idObject: LongWord; idChild: LongWord;
                         idProp: TGUID; var_: OleVariant): HResult; stdcall;
    function SetHwndPropStr(var hwnd: _RemotableHandle; idObject: LongWord; idChild: LongWord;
                            idProp: TGUID; str: PWideChar): HResult; stdcall;
    function SetHwndPropServer(var hwnd: _RemotableHandle; idObject: LongWord; idChild: LongWord;
                               var paProps: TGUID; cProps: SYSINT; const pServer: IAccPropServer;
                               AnnoScope: AnnoScope): HResult; stdcall;
    function ClearHwndProps(var hwnd: _RemotableHandle; idObject: LongWord; idChild: LongWord;
                            var paProps: TGUID; cProps: SYSINT): HResult; stdcall;
    function ComposeHwndIdentityString(var hwnd: _RemotableHandle; idObject: LongWord;
                                       idChild: LongWord; out ppIDString: PByte1;
                                       out pdwIDStringLen: LongWord): HResult; stdcall;
    function DecomposeHwndIdentityString(var pIDString: Byte; dwIDStringLen: LongWord;
                                         out phwnd: wireHWND; out pidObject: LongWord;
                                         out pidChild: LongWord): HResult; stdcall;
    function SetHmenuProp(var hmenu: _RemotableHandle; idChild: LongWord; idProp: TGUID;
                          var_: OleVariant): HResult; stdcall;
    function SetHmenuPropStr(var hmenu: _RemotableHandle; idChild: LongWord; idProp: TGUID;
                             str: PWideChar): HResult; stdcall;
    function SetHmenuPropServer(var hmenu: _RemotableHandle; idChild: LongWord; var paProps: TGUID;
                                cProps: SYSINT; const pServer: IAccPropServer; AnnoScope: AnnoScope): HResult; stdcall;
    function ClearHmenuProps(var hmenu: _RemotableHandle; idChild: LongWord; var paProps: TGUID;
                             cProps: SYSINT): HResult; stdcall;
    function ComposeHmenuIdentityString(var hmenu: _RemotableHandle; idChild: LongWord;
                                        out ppIDString: PByte1; out pdwIDStringLen: LongWord): HResult; stdcall;
    function DecomposeHmenuIdentityString(var pIDString: Byte; dwIDStringLen: LongWord;
                                          out phmenu: wireHMENU; out pidChild: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// The Class CoCAccPropServices provides a Create and CreateRemote method to
// create instances of the default interface IAccPropServices exposed by
// the CoClass CAccPropServices. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  {$EXTERNALSYM CoCAccPropServices}
  CoCAccPropServices = class
    class function Create: IAccPropServices;
    class function CreateRemote(const MachineName: string): IAccPropServices;
  end;

{$EXTERNALSYM CreateAccessibleProxy}
function CreateAccessibleProxy(Handle: THandle; const Classname: string): IDispatch; inline;

{$EXTERNALSYM LresultFromObject}
function LresultFromObject(const riid: TGUID; wParam: WPARAM; punk: IUnknown): LRESULT; stdcall;
{$EXTERNALSYM ObjectFromLresult}
function ObjectFromLresult(lResult: LRESULT; const riid: TGUID; wParam: WPARAM; out ppvObject): LRESULT; stdcall;
{$EXTERNALSYM WindowFromAccessibleObject}
function WindowFromAccessibleObject(Accessible: IAccessible; phwnd: PHandle): HRESULT; stdcall; overload;
function WindowFromAccessibleObject(Accessible: IAccessible; var hwnd: THandle): HRESULT; stdcall; overload;
{$EXTERNALSYM AccessibleObjectFromWindow}
function AccessibleObjectFromWindow(hwnd: THandle; dwId: DWORD; const riid: TGUID; out ppvObject): HRESULT; stdcall;
function AccessibleObjectFromEvent(hwnd: THandle; swId: DWORD; dwChildId: DWORD; out ppacc: IAccessible; out pvarChild: VARIANT): HRESULT; stdcall;
{$EXTERNALSYM AccessibleObjectFromEvent}
function AccessibleObjectFromPoint(ptScreen: TPoint; out ppacc: IAccessible; out pvarChild: VARIANT): HRESULT; stdcall;
{$EXTERNALSYM AccessibleObjectFromPoint}
function AccessibleChildren(paccContainer: IAccessible; iChildStart: LONG; cChildren: LONG; out rgvarChildren:  VARIANT; out pcObtained: LONG): HRESULT; stdcall;
{$EXTERNALSYM AccessibleChildren}

{$EXTERNALSYM GetRoleText}
function GetRoleText(lRole: DWORD; lpszRole: LPWSTR; cchRoleMax: UINT): HRESULT; stdcall;
{$EXTERNALSYM GetRoleTextA}
function GetRoleTextA(lRole: DWORD; lpszRole: LPSTR; cchRoleMax: UINT): HRESULT; stdcall;
{$EXTERNALSYM GetRoleTextW}
function GetRoleTextW(lRole: DWORD; lpszRole: LPWSTR; cchRoleMax: UINT): HRESULT; stdcall;
{$EXTERNALSYM GetStateText}
function GetStateText(lStateBit: DWORD; lpszState: LPWSTR; cchState: UINT): HRESULT; stdcall;
{$EXTERNALSYM GetStateTextA}
function GetStateTextA(lStateBit: DWORD; lpszState: LPSTR; cchState: UINT): HRESULT; stdcall;
{$EXTERNALSYM GetStateTextW}
function GetStateTextW(lStateBit: DWORD; lpszState: LPWSTR; cchState: UINT): HRESULT; stdcall;

{$EXTERNALSYM GetOleaccVersionInfo}
function GetOleaccVersionInfo(var Ver: DWORD; var Build: DWORD): HRESULT; stdcall;

{$EXTERNALSYM CreateStdAccessibleObject}
function CreateStdAccessibleObject(hwnd: THandle; idObject: LongInt; const riid: TGUID; out ppvObject: Pointer): HRESULT; stdcall;
{$EXTERNALSYM CreateStdAccessibleProxy}
function CreateStdAccessibleProxy(hwnd: THandle; pClassName: LPCWSTR; idObject: LongInt; const riid: TGUID; out ppvObject): HRESULT; stdcall;
{$EXTERNALSYM CreateStdAccessibleProxyA}
function CreateStdAccessibleProxyA(hwnd: THandle; pClassName: LPCSTR; idObject: LongInt; const riid: TGUID; out ppvObject): HRESULT; stdcall;
{$EXTERNALSYM CreateStdAccessibleProxyW}
function CreateStdAccessibleProxyW(hwnd: THandle; pClassName: LPCWSTR; idObject: LongInt; const riid: TGUID; out ppvObject): HRESULT; stdcall;

{$EXTERNALSYM AccSetRunningUtilityState}
function AccSetRunningUtilityState(): HRESULT; stdcall;
{$EXTERNALSYM AccNotifyTouchInteraction}
function AccNotifyTouchInteraction(): HRESULT; stdcall;

implementation

uses
  System.Win.ComObj;

const
  ModName = 'oleacc.dll';

function LresultFromObject; external ModName name 'LresultFromObject';
function ObjectFromLresult; external ModName name 'ObjectFromLresult';
function WindowFromAccessibleObject(Accessible: IAccessible; phwnd: PHandle): HRESULT; stdcall; overload; external ModName name 'WindowFromAccessibleObject';
function WindowFromAccessibleObject(Accessible: IAccessible; var hwnd: THandle): HRESULT; stdcall; overload; external ModName name 'WindowFromAccessibleObject';
function AccessibleObjectFromWindow; external ModName name 'AccessibleObjectFromWindow';
function AccessibleObjectFromEvent; external ModName name 'AccessibleObjectFromEvent';
function AccessibleObjectFromPoint; external ModName name 'AccessibleObjectFromPoint';
function AccessibleChildren; external ModName name 'AccessibleChildren';

function GetRoleText; external ModName name 'GetRoleTextW';
function GetRoleTextA; external ModName name 'GetRoleTextA';
function GetRoleTextW; external ModName name 'GetRoleTextW';
function GetStateText; external ModName name 'GetStateTextW';
function GetStateTextA; external ModName name 'GetStateTextA';
function GetStateTextW; external ModName name 'GetStateTextW';
function GetOleaccVersionInfo; external ModName name 'GetOleaccVersionInfo';
function CreateStdAccessibleObject; external ModName name 'CreateStdAccessibleObject';
function CreateStdAccessibleProxy; external ModName name 'CreateStdAccessibleProxyW';
function CreateStdAccessibleProxyA; external ModName name 'CreateStdAccessibleProxyA';
function CreateStdAccessibleProxyW; external ModName name 'CreateStdAccessibleProxyW';
function AccSetRunningUtilityState; external ModName name 'AccSetRunningUtilityState';
function AccNotifyTouchInteraction; external ModName name 'AccNotifyTouchInteraction';

function CreateAccessibleProxy(Handle: THandle; const Classname: string): IDispatch;
begin
  CreateStdAccessibleProxy(Handle, PChar(Classname), Longint(OBJID_CLIENT), IID_IAccessible, Result);  { do not localize }
end;

class function CoCAccPropServices.Create: IAccPropServices;
begin
  Result := CreateComObject(CLSID_AccPropServices) as IAccPropServices;
end;

class function CoCAccPropServices.CreateRemote(const MachineName: string): IAccPropServices;
begin
  Result := CreateRemoteComObject(MachineName, CLSID_AccPropServices) as IAccPropServices;
end;

end.
