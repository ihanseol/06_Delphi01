{*******************************************************}
{                                                       }
{                Delphi Runtime Library                 }
{                                                       }
{          File: richedit.h                             }
{          Copyright (c) Microsoft Corporation          }
{          All Rights Reserved.                         }
{                                                       }
{       Translator: Embarcadero Technologies, Inc.      }
{ Copyright(c) 1995-2024 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

{*******************************************************}
{       Win32 Rich Edit control Interface Unit          }
{*******************************************************}

{$HPPEMIT '#include <RichEdit.h>'}

unit Winapi.RichEdit;

{$ALIGN 4}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}

interface

uses Winapi.Messages, Winapi.Windows;

const
  {$EXTERNALSYM cchTextLimitDefault}
  cchTextLimitDefault     = 32767;


  {$EXTERNALSYM MSFTEDIT_CLASS}
  MSFTEDIT_CLASS       = 'RICHEDIT50W';        { Richedit 4.1 }
  {$EXTERNALSYM RICHEDIT_CLASSA}
  RICHEDIT_CLASSA       = 'RichEdit20A';     { Richedit2.0 Window Class. }
  {$EXTERNALSYM RICHEDIT_CLASSW}
  RICHEDIT_CLASSW       = 'RichEdit20W';     { Richedit2.0 Window Class. }
  {$EXTERNALSYM RICHEDIT_CLASS}
  RICHEDIT_CLASS = RICHEDIT_CLASSW;
  {$EXTERNALSYM RICHEDIT_CLASS10A}
  RICHEDIT_CLASS10A       = 'RICHEDIT';        { Richedit 1.0 }

{ RichEdit messages }

//  EM_GETLIMITTEXT                     = WM_USER + 37;
//  EM_POSFROMCHAR                      = WM_USER + 38;
//  EM_CHARFROMPOS                      = WM_USER + 39;
//  EM_SCROLLCARET                      = WM_USER + 49;
  {$EXTERNALSYM EM_CANPASTE}
  EM_CANPASTE                         = WM_USER + 50;
  {$EXTERNALSYM EM_DISPLAYBAND}
  EM_DISPLAYBAND                      = WM_USER + 51;
  {$EXTERNALSYM EM_EXGETSEL}
  EM_EXGETSEL                         = WM_USER + 52;
  {$EXTERNALSYM EM_EXLIMITTEXT}
  EM_EXLIMITTEXT                      = WM_USER + 53;
  {$EXTERNALSYM EM_EXLINEFROMCHAR}
  EM_EXLINEFROMCHAR                   = WM_USER + 54;
  {$EXTERNALSYM EM_EXSETSEL}
  EM_EXSETSEL                         = WM_USER + 55;
  {$EXTERNALSYM EM_FINDTEXT}
  EM_FINDTEXT                         = WM_USER + 56;
  {$EXTERNALSYM EM_FORMATRANGE}
  EM_FORMATRANGE                      = WM_USER + 57;
  {$EXTERNALSYM EM_GETCHARFORMAT}
  EM_GETCHARFORMAT                    = WM_USER + 58;
  {$EXTERNALSYM EM_GETEVENTMASK}
  EM_GETEVENTMASK                     = WM_USER + 59;
  {$EXTERNALSYM EM_GETOLEINTERFACE}
  EM_GETOLEINTERFACE                  = WM_USER + 60;
  {$EXTERNALSYM EM_GETPARAFORMAT}
  EM_GETPARAFORMAT                    = WM_USER + 61;
  {$EXTERNALSYM EM_GETSELTEXT}
  EM_GETSELTEXT                       = WM_USER + 62;
  {$EXTERNALSYM EM_HIDESELECTION}
  EM_HIDESELECTION                    = WM_USER + 63;
  {$EXTERNALSYM EM_PASTESPECIAL}
  EM_PASTESPECIAL                     = WM_USER + 64;
  {$EXTERNALSYM EM_REQUESTRESIZE}
  EM_REQUESTRESIZE                    = WM_USER + 65;
  {$EXTERNALSYM EM_SELECTIONTYPE}
  EM_SELECTIONTYPE                    = WM_USER + 66;
  {$EXTERNALSYM EM_SETBKGNDCOLOR}
  EM_SETBKGNDCOLOR                    = WM_USER + 67;
  {$EXTERNALSYM EM_SETCHARFORMAT}
  EM_SETCHARFORMAT                    = WM_USER + 68;
  {$EXTERNALSYM EM_SETEVENTMASK}
  EM_SETEVENTMASK                     = WM_USER + 69;
  {$EXTERNALSYM EM_SETOLECALLBACK}
  EM_SETOLECALLBACK                   = WM_USER + 70;
  {$EXTERNALSYM EM_SETPARAFORMAT}
  EM_SETPARAFORMAT                    = WM_USER + 71;
  {$EXTERNALSYM EM_SETTARGETDEVICE}
  EM_SETTARGETDEVICE                  = WM_USER + 72;
  {$EXTERNALSYM EM_STREAMIN}
  EM_STREAMIN                         = WM_USER + 73;
  {$EXTERNALSYM EM_STREAMOUT}
  EM_STREAMOUT                        = WM_USER + 74;
  {$EXTERNALSYM EM_GETTEXTRANGE}
  EM_GETTEXTRANGE                     = WM_USER + 75;
  {$EXTERNALSYM EM_FINDWORDBREAK}
  EM_FINDWORDBREAK                    = WM_USER + 76;
  {$EXTERNALSYM EM_SETOPTIONS}
  EM_SETOPTIONS                       = WM_USER + 77;
  {$EXTERNALSYM EM_GETOPTIONS}
  EM_GETOPTIONS                       = WM_USER + 78;
  {$EXTERNALSYM EM_FINDTEXTEX}
  EM_FINDTEXTEX                       = WM_USER + 79;
  {$EXTERNALSYM EM_GETWORDBREAKPROCEX}
  EM_GETWORDBREAKPROCEX               = WM_USER + 80;
  {$EXTERNALSYM EM_SETWORDBREAKPROCEX}
  EM_SETWORDBREAKPROCEX               = WM_USER + 81;

{ Richedit v2.0 messages }

  {$EXTERNALSYM EM_SETUNDOLIMIT}
  EM_SETUNDOLIMIT                     = WM_USER + 82;
  {$EXTERNALSYM EM_REDO}
  EM_REDO                             = WM_USER + 84;
  {$EXTERNALSYM EM_CANREDO}
  EM_CANREDO                          = WM_USER + 85;
  {$EXTERNALSYM EM_GETUNDONAME}
  EM_GETUNDONAME                      = WM_USER + 86;
  {$EXTERNALSYM EM_GETREDONAME}
  EM_GETREDONAME                      = WM_USER + 87;
  {$EXTERNALSYM EM_STOPGROUPTYPING}
  EM_STOPGROUPTYPING                  = WM_USER + 88;
  {$EXTERNALSYM EM_SETTEXTMODE}
  EM_SETTEXTMODE                      = WM_USER + 89;
  {$EXTERNALSYM EM_GETTEXTMODE}
  EM_GETTEXTMODE                      = WM_USER + 90;

{ for use with EM_GET/SETTEXTMODE }

  TM_PLAINTEXT                       = 1;
  {$EXTERNALSYM TM_PLAINTEXT}
  TM_RICHTEXT                        = 2;     { default behavior }
  {$EXTERNALSYM TM_RICHTEXT}
  TM_SINGLELEVELUNDO                 = 4;
  {$EXTERNALSYM TM_SINGLELEVELUNDO}
  TM_MULTILEVELUNDO                  = 8;     { default behavior }
  {$EXTERNALSYM TM_MULTILEVELUNDO}
  TM_SINGLECODEPAGE                  = 16;
  {$EXTERNALSYM TM_SINGLECODEPAGE}
  TM_MULTICODEPAGE                   = 32;    { default behavior }
  {$EXTERNALSYM TM_MULTICODEPAGE}

  {$EXTERNALSYM EM_AUTOURLDETECT}
  EM_AUTOURLDETECT                    = WM_USER + 91;
  {$EXTERNALSYM EM_GETAUTOURLDETECT}
  EM_GETAUTOURLDETECT                 = WM_USER + 92;
  {$EXTERNALSYM EM_SETPALETTE}
  EM_SETPALETTE                       = WM_USER + 93;
  {$EXTERNALSYM EM_GETTEXTEX}
  EM_GETTEXTEX                        = WM_USER + 94;
  {$EXTERNALSYM EM_GETTEXTLENGTHEX}
  EM_GETTEXTLENGTHEX                  = WM_USER + 95;

{ Far East specific messages }

  {$EXTERNALSYM EM_SETPUNCTUATION}
  EM_SETPUNCTUATION                   = WM_USER + 100;
  {$EXTERNALSYM EM_GETPUNCTUATION}
  EM_GETPUNCTUATION                   = WM_USER + 101;
  {$EXTERNALSYM EM_SETWORDWRAPMODE}
  EM_SETWORDWRAPMODE                  = WM_USER + 102;
  {$EXTERNALSYM EM_GETWORDWRAPMODE}
  EM_GETWORDWRAPMODE                  = WM_USER + 103;
  {$EXTERNALSYM EM_SETIMECOLOR}
  EM_SETIMECOLOR                      = WM_USER + 104;
  {$EXTERNALSYM EM_GETIMECOLOR}
  EM_GETIMECOLOR                      = WM_USER + 105;
  {$EXTERNALSYM EM_SETIMEOPTIONS}
  EM_SETIMEOPTIONS                    = WM_USER + 106;
  {$EXTERNALSYM EM_GETIMEOPTIONS}
  EM_GETIMEOPTIONS                    = WM_USER + 107;
  {$EXTERNALSYM EM_CONVPOSITION}
  EM_CONVPOSITION                     = WM_USER + 108;

  {$EXTERNALSYM EM_SETLANGOPTIONS}
  EM_SETLANGOPTIONS                   = WM_USER + 120;
  {$EXTERNALSYM EM_GETLANGOPTIONS}
  EM_GETLANGOPTIONS                   = WM_USER + 121;
  {$EXTERNALSYM EM_GETIMECOMPMODE}
  EM_GETIMECOMPMODE                   = WM_USER + 122;

  EM_FINDTEXTW                        = WM_USER + 123;
  {$EXTERNALSYM EM_FINDTEXTW}
  EM_FINDTEXTEXW                      = WM_USER + 124;
  {$EXTERNALSYM EM_FINDTEXTEXW}

{ RE3.0 FE messages }

  {$EXTERNALSYM EM_RECONVERSION}
  EM_RECONVERSION                     = WM_USER + 125;
  {$EXTERNALSYM EM_SETIMEMODEBIAS}
  EM_SETIMEMODEBIAS                   = WM_USER + 126;
  {$EXTERNALSYM EM_GETIMEMODEBIAS}
  EM_GETIMEMODEBIAS                   = WM_USER + 127;

{ BiDi specific messages }

  {$EXTERNALSYM EM_SETBIDIOPTIONS}
  EM_SETBIDIOPTIONS                   = WM_USER + 200;
  {$EXTERNALSYM EM_GETBIDIOPTIONS}
  EM_GETBIDIOPTIONS                   = WM_USER + 201;

  {$EXTERNALSYM EM_SETTYPOGRAPHYOPTIONS}
  EM_SETTYPOGRAPHYOPTIONS             = WM_USER + 202;
  {$EXTERNALSYM EM_GETTYPOGRAPHYOPTIONS}
  EM_GETTYPOGRAPHYOPTIONS             = WM_USER + 203;

{ Extended edit style specific messages }

  {$EXTERNALSYM EM_SETEDITSTYLE}
  EM_SETEDITSTYLE                     = WM_USER + 204;
  {$EXTERNALSYM EM_GETEDITSTYLE}
  EM_GETEDITSTYLE                     = WM_USER + 205;

{ Pegasus outline mode messages (RE 3.0) }

  {$EXTERNALSYM EM_OUTLINE}
  EM_OUTLINE                          = WM_USER + 220;
  {$EXTERNALSYM EM_GETSCROLLPOS}
  EM_GETSCROLLPOS                     = WM_USER + 221;
  {$EXTERNALSYM EM_SETSCROLLPOS}
  EM_SETSCROLLPOS                     = WM_USER + 222;
  {$EXTERNALSYM EM_SETFONTSIZE}
  EM_SETFONTSIZE                      = WM_USER + 223;
  {$EXTERNALSYM EM_GETZOOM}
  EM_GETZOOM                          = WM_USER + 224;
  {$EXTERNALSYM EM_SETZOOM}
  EM_SETZOOM                          = WM_USER + 225;

  {$EXTERNALSYM EM_GETVIEWKIND}
  EM_GETVIEWKIND                      = WM_USER + 226;
  {$EXTERNALSYM EM_SETVIEWKIND}
  EM_SETVIEWKIND                      = WM_USER + 227;

// RichEdit 4.0 messages
  {$EXTERNALSYM EM_GETPAGE}
  EM_GETPAGE                          = WM_USER + 228;
  {$EXTERNALSYM EM_SETPAGE}
  EM_SETPAGE                          = WM_USER + 229;
  {$EXTERNALSYM EM_GETHYPHENATEINFO}
  EM_GETHYPHENATEINFO                 = WM_USER + 230;
  {$EXTERNALSYM EM_SETHYPHENATEINFO}
  EM_SETHYPHENATEINFO                 = WM_USER + 231;

  {$EXTERNALSYM EM_GETPAGEROTATE}
  EM_GETPAGEROTATE                    = WM_USER + 235;
  {$EXTERNALSYM EM_SETPAGEROTATE}
  EM_SETPAGEROTATE                    = WM_USER + 236;
  {$EXTERNALSYM EM_GETCTFMODEBIAS}
  EM_GETCTFMODEBIAS                   = WM_USER + 237;
  {$EXTERNALSYM EM_SETCTFMODEBIAS}
  EM_SETCTFMODEBIAS                   = WM_USER + 238;
  {$EXTERNALSYM EM_GETCTFOPENSTATUS}
  EM_GETCTFOPENSTATUS                 = WM_USER + 240;
  {$EXTERNALSYM EM_SETCTFOPENSTATUS}
  EM_SETCTFOPENSTATUS                 = WM_USER + 241;
  {$EXTERNALSYM EM_GETIMECOMPTEXT}
  EM_GETIMECOMPTEXT                   = WM_USER + 242;
  {$EXTERNALSYM EM_ISIME}
  EM_ISIME                            = WM_USER + 243;
  {$EXTERNALSYM EM_GETIMEPROPERTY}
  EM_GETIMEPROPERTY                   = WM_USER + 244;
  {$EXTERNALSYM EM_GETQUERYRTFOBJ}
  EM_GETQUERYRTFOBJ                   = WM_USER + 269;
  {$EXTERNALSYM EM_SETQUERYRTFOBJ}
  EM_SETQUERYRTFOBJ                   = WM_USER + 270;

{ EM_SETPAGEROTATE wparam values }

  {$EXTERNALSYM EPR_0}
  EPR_0    = 0;
  {$EXTERNALSYM EPR_270}
  EPR_270 = 1;
  {$EXTERNALSYM EPR_180}
  EPR_180 = 2;
  {$EXTERNALSYM EPR_90}
  EPR_90  = 3;
  {$EXTERNALSYM EPR_SE}
  EPR_SE  = 5;

{  EM_SETCTFMODEBIAS wparam values }

  {$EXTERNALSYM CTFMODEBIAS_DEFAULT}
  CTFMODEBIAS_DEFAULT               = $0000;
  {$EXTERNALSYM CTFMODEBIAS_FILENAME}
  CTFMODEBIAS_FILENAME              = $0001;
  {$EXTERNALSYM CTFMODEBIAS_NAME}
  CTFMODEBIAS_NAME                  = $0002;
  {$EXTERNALSYM CTFMODEBIAS_READING}
  CTFMODEBIAS_READING               = $0003;
  {$EXTERNALSYM CTFMODEBIAS_DATETIME}
  CTFMODEBIAS_DATETIME              = $0004;
  {$EXTERNALSYM CTFMODEBIAS_CONVERSATION}
  CTFMODEBIAS_CONVERSATION          = $0005;
  {$EXTERNALSYM CTFMODEBIAS_NUMERIC}
  CTFMODEBIAS_NUMERIC               = $0006;
  {$EXTERNALSYM CTFMODEBIAS_HIRAGANA}
  CTFMODEBIAS_HIRAGANA              = $0007;
  {$EXTERNALSYM CTFMODEBIAS_KATAKANA}
  CTFMODEBIAS_KATAKANA              = $0008;
  {$EXTERNALSYM CTFMODEBIAS_HANGUL}
  CTFMODEBIAS_HANGUL                = $0009;
  {$EXTERNALSYM CTFMODEBIAS_HALFWIDTHKATAKANA}
  CTFMODEBIAS_HALFWIDTHKATAKANA     = $000A;
  {$EXTERNALSYM CTFMODEBIAS_FULLWIDTHALPHANUMERIC}
  CTFMODEBIAS_FULLWIDTHALPHANUMERIC = $000B;
  {$EXTERNALSYM CTFMODEBIAS_HALFWIDTHALPHANUMERIC}
  CTFMODEBIAS_HALFWIDTHALPHANUMERIC = $000C;

{ EM_SETIMEMODEBIAS lparam values }

  {$EXTERNALSYM IMF_SMODE_PLAURALCLAUSE}
  IMF_SMODE_PLAURALCLAUSE = $0001;
  {$EXTERNALSYM IMF_SMODE_NONE}
  IMF_SMODE_NONE          = $0002;

{ EM_GETIMECOMPTEXT wparam structure }
type
  {$EXTERNALSYM IMECOMPTEXT}
  IMECOMPTEXT = record
    cb: Longint;  // count of bytes in the output buffer.
    flags: DWORD; // value specifying the composition string type.
                  // Currently only support ICT_RESULTREADSTR
end;

const
  {$EXTERNALSYM ICT_RESULTREADSTR}
  ICT_RESULTREADSTR = 1;

{ Outline mode wparam values }

  {$EXTERNALSYM EMO_EXIT}
  EMO_EXIT          = 0;
  {$EXTERNALSYM EMO_ENTER}
  EMO_ENTER         = 1;
  {$EXTERNALSYM EMO_PROMOTE}
  EMO_PROMOTE       = 2;
  {$EXTERNALSYM EMO_EXPAND}
  EMO_EXPAND        = 3;
  {$EXTERNALSYM EMO_MOVESELECTION}
  EMO_MOVESELECTION = 4;
  {$EXTERNALSYM EMO_GETVIEWMODE}
  EMO_GETVIEWMODE   = 5;

{ EMO_EXPAND options }

  {$EXTERNALSYM EMO_EXPANDSELECTION}
  EMO_EXPANDSELECTION = 0;
  {$EXTERNALSYM EMO_EXPANDDOCUMENT}
  EMO_EXPANDDOCUMENT  = 1;

  {$EXTERNALSYM VM_NORMAL}
  VM_NORMAL  = 4;
  {$EXTERNALSYM VM_OUTLINE}
  VM_OUTLINE = 2;
  {$EXTERNALSYM VM_PAGE}
  VM_PAGE    = 9;

{  Extended edit style masks }

  {$EXTERNALSYM SES_EMULATESYSEDIT}
  SES_EMULATESYSEDIT                  = 1;
  {$EXTERNALSYM SES_BEEPONMAXTEXT}
  SES_BEEPONMAXTEXT                   = 2;
  {$EXTERNALSYM SES_EXTENDBACKCOLOR}
  SES_EXTENDBACKCOLOR                 = 4;
  {$EXTERNALSYM SES_MAPCPS}
  SES_MAPCPS                          = 8;  // (obsolete)
  {$EXTERNALSYM SES_HYPERLINKTOOLTIPS}
  SES_HYPERLINKTOOLTIPS               = 8;  // RICHEDIT_VER >= 0x0500
  {$EXTERNALSYM SES_EMULATE10}
  SES_EMULATE10                       = 16; // (obsolete)
  {$EXTERNALSYM SES_DEFAULTLATINLIGA}
  SES_DEFAULTLATINLIGA                = 16; // RICHEDIT_VER >= 0x0700
  {$EXTERNALSYM SES_USECRLF}
  SES_USECRLF                         = 32; // (obsolete)
  {$EXTERNALSYM SES_NOFOCUSLINKNOTIFY}
  SES_NOFOCUSLINKNOTIFY               = 32; // RICHEDIT_VER >= 0x0700
  SES_NOXLTSYMBOLRANGE                = 32;
  {$EXTERNALSYM SES_USEAIMM}
  SES_USEAIMM                         = 64;
  {$EXTERNALSYM SES_NOIME}
  SES_NOIME                           = 128;

  {$EXTERNALSYM SES_ALLOWBEEPS}
  SES_ALLOWBEEPS                      = 256;
  {$EXTERNALSYM SES_UPPERCASE}
  SES_UPPERCASE                       = 512;
  {$EXTERNALSYM SES_LOWERCASE}
  SES_LOWERCASE                       = 1024;
  {$EXTERNALSYM SES_NOINPUTSEQUENCECHK}
  SES_NOINPUTSEQUENCECHK              = 2048;
  {$EXTERNALSYM SES_BIDI}
  SES_BIDI                            = 4096;
  {$EXTERNALSYM SES_SCROLLONKILLFOCUS}
  SES_SCROLLONKILLFOCUS               = 8192;
  {$EXTERNALSYM SES_XLTCRCRLFTOCR}
  SES_XLTCRCRLFTOCR                   = 16384;
  {$EXTERNALSYM SES_DRAFTMODE}
  SES_DRAFTMODE                       = 32768;

  {$EXTERNALSYM SES_USECTF}
  SES_USECTF                          = $0010000;
  {$EXTERNALSYM SES_HIDEGRIDLINES}
  SES_HIDEGRIDLINES                   = $0020000;
  {$EXTERNALSYM SES_USEATFONT}
  SES_USEATFONT                       = $0040000;
  {$EXTERNALSYM SES_CUSTOMLOOK}
  SES_CUSTOMLOOK                      = $0080000;
  {$EXTERNALSYM SES_LBSCROLLNOTIFY}
  SES_LBSCROLLNOTIFY                  = $0100000;
  {$EXTERNALSYM SES_CTFALLOWEMBED}
  SES_CTFALLOWEMBED                   = $0200000;
  {$EXTERNALSYM SES_CTFALLOWSMARTTAG}
  SES_CTFALLOWSMARTTAG                = $0400000;
  {$EXTERNALSYM SES_CTFALLOWPROOFING}
  SES_CTFALLOWPROOFING                = $0800000;

{ Options for EM_SETLANGOPTIONS and EM_GETLANGOPTIONS }

  {$EXTERNALSYM IMF_AUTOKEYBOARD}
  IMF_AUTOKEYBOARD            = $0001;
  {$EXTERNALSYM IMF_AUTOFONT}
  IMF_AUTOFONT                = $0002;
  {$EXTERNALSYM IMF_IMECANCELCOMPLETE}
  IMF_IMECANCELCOMPLETE       = $0004;  { high completes the comp string when aborting, low cancels. }
  {$EXTERNALSYM IMF_IMEALWAYSSENDNOTIFY}
  IMF_IMEALWAYSSENDNOTIFY     = $0008;
  {$EXTERNALSYM IMF_AUTOFONTSIZEADJUST}
  IMF_AUTOFONTSIZEADJUST      = $0010;
  {$EXTERNALSYM IMF_UIFONTS}
  IMF_UIFONTS                 = $0020;
  {$EXTERNALSYM IMF_NOIMPLICITLANG}
  IMF_NOIMPLICITLANG          = $0040; { RICHEDIT_VER >= 0x0800 }
  {$EXTERNALSYM IMF_DUALFONT}
  IMF_DUALFONT                = $0080;
  {$EXTERNALSYM IMF_NOKBDLIDFIXUP}
  IMF_NOKBDLIDFIXUP           = $0200; { RICHEDIT_VER >= 0x0800 }
  {$EXTERNALSYM IMF_NORTFFONTSUBSTITUTE}
  IMF_NORTFFONTSUBSTITUTE     = $0400;
  {$EXTERNALSYM IMF_SPELLCHECKING}
  IMF_SPELLCHECKING           = $0800; { RICHEDIT_VER >= 0x0800 }
  {$EXTERNALSYM IMF_TKBPREDICTION}
  IMF_TKBPREDICTION           = $1000; { RICHEDIT_VER >= 0x0800 }
  {$EXTERNALSYM IMF_IMEUIINTEGRATION}
  IMF_IMEUIINTEGRATION        = $2000; { RICHEDIT_VER >= 0x0810 }

{ Values for EM_GETIMECOMPMODE }

  {$EXTERNALSYM ICM_NOTOPEN}
  ICM_NOTOPEN                         = $0000;
  {$EXTERNALSYM ICM_LEVEL3}
  ICM_LEVEL3                          = $0001;
  {$EXTERNALSYM ICM_LEVEL2}
  ICM_LEVEL2                          = $0002;
  {$EXTERNALSYM ICM_LEVEL2_5}
  ICM_LEVEL2_5                        = $0003;
  {$EXTERNALSYM ICM_LEVEL2_SUI}
  ICM_LEVEL2_SUI                      = $0004;

{ New notifications }

  {$EXTERNALSYM EN_MSGFILTER}
  EN_MSGFILTER                        = $0700;
  {$EXTERNALSYM EN_REQUESTRESIZE}
  EN_REQUESTRESIZE                    = $0701;
  {$EXTERNALSYM EN_SELCHANGE}
  EN_SELCHANGE                        = $0702;
  {$EXTERNALSYM EN_DROPFILES}
  EN_DROPFILES                        = $0703;
  {$EXTERNALSYM EN_PROTECTED}
  EN_PROTECTED                        = $0704;
  {$EXTERNALSYM EN_CORRECTTEXT}
  EN_CORRECTTEXT                      = $0705;                  { PenWin specific }
  {$EXTERNALSYM EN_STOPNOUNDO}
  EN_STOPNOUNDO                       = $0706;
  {$EXTERNALSYM EN_IMECHANGE}
  EN_IMECHANGE                        = $0707;                  { Far East specific }
  {$EXTERNALSYM EN_SAVECLIPBOARD}
  EN_SAVECLIPBOARD                    = $0708;
  {$EXTERNALSYM EN_OLEOPFAILED}
  EN_OLEOPFAILED                      = $0709;
  {$EXTERNALSYM EN_OBJECTPOSITIONS}
  EN_OBJECTPOSITIONS                  = $070a;
  {$EXTERNALSYM EN_LINK}
  EN_LINK                             = $070b;
  {$EXTERNALSYM EN_DRAGDROPDONE}
  EN_DRAGDROPDONE                     = $070c;

{ Event notification masks }

  {$EXTERNALSYM ENM_NONE}
  ENM_NONE                            = $00000000;
  {$EXTERNALSYM ENM_CHANGE}
  ENM_CHANGE                          = $00000001;
  {$EXTERNALSYM ENM_UPDATE}
  ENM_UPDATE                          = $00000002;
  {$EXTERNALSYM ENM_SCROLL}
  ENM_SCROLL                          = $00000004;
  {$EXTERNALSYM ENM_KEYEVENTS}
  ENM_KEYEVENTS                       = $00010000;
  {$EXTERNALSYM ENM_MOUSEEVENTS}
  ENM_MOUSEEVENTS                     = $00020000;
  {$EXTERNALSYM ENM_REQUESTRESIZE}
  ENM_REQUESTRESIZE                   = $00040000;
  {$EXTERNALSYM ENM_SELCHANGE}
  ENM_SELCHANGE                       = $00080000;
  {$EXTERNALSYM ENM_DROPFILES}
  ENM_DROPFILES                       = $00100000;
  {$EXTERNALSYM ENM_PROTECTED}
  ENM_PROTECTED                       = $00200000;
  {$EXTERNALSYM ENM_CORRECTTEXT}
  ENM_CORRECTTEXT                     = $00400000;              { PenWin specific }
  {$EXTERNALSYM ENM_SCROLLEVENTS}
  ENM_SCROLLEVENTS                    = $00000008;
  {$EXTERNALSYM ENM_DRAGDROPDONE}
  ENM_DRAGDROPDONE                    = $00000010;

{ Far East specific notification mask }

  {$EXTERNALSYM ENM_IMECHANGE}
  ENM_IMECHANGE                       = $00800000;              { unused by RE2.0 }
  {$EXTERNALSYM ENM_LANGCHANGE}
  ENM_LANGCHANGE                      = $01000000;
  {$EXTERNALSYM ENM_OBJECTPOSITIONS}
  ENM_OBJECTPOSITIONS                 = $02000000;
  {$EXTERNALSYM ENM_LINK}
  ENM_LINK                            = $04000000;

{ New edit control styles }

  {$EXTERNALSYM ES_SAVESEL}
  ES_SAVESEL                          = $00008000;
  {$EXTERNALSYM ES_SUNKEN}
  ES_SUNKEN                           = $00004000;
  {$EXTERNALSYM ES_DISABLENOSCROLL}
  ES_DISABLENOSCROLL                  = $00002000;
{ same as WS_MAXIMIZE, but that doesn't make sense so we re-use the value }
  {$EXTERNALSYM ES_SELECTIONBAR}
  ES_SELECTIONBAR                     = $01000000;
{ same as ES_UPPERCASE, but re-used to completely disable OLE drag'n'drop }
  {$EXTERNALSYM ES_NOOLEDRAGDROP}
  ES_NOOLEDRAGDROP                    = $00000008;

{ New edit control extended style }

  {$EXTERNALSYM ES_EX_NOCALLOLEINIT}
  ES_EX_NOCALLOLEINIT                 = $00000000; { ifdef _WIN32_WINNT > 0x0400 || WINVER > 0x0400 - Not supported in RE 2.0/3.0 }
  //ES_EX_NOCALLOLEINIT                 = $01000000; { else if _WIN32 }

{ These flags are used in FE Windows }

  {$EXTERNALSYM ES_VERTICAL}
  ES_VERTICAL                         = $00400000;
  {$EXTERNALSYM ES_NOIME}
  ES_NOIME                            = $00080000;
  {$EXTERNALSYM ES_SELFIME}
  ES_SELFIME                          = $00040000;

{ Edit control options }

  {$EXTERNALSYM ECO_AUTOWORDSELECTION}
  ECO_AUTOWORDSELECTION       = $00000001;
  {$EXTERNALSYM ECO_AUTOVSCROLL}
  ECO_AUTOVSCROLL             = $00000040;
  {$EXTERNALSYM ECO_AUTOHSCROLL}
  ECO_AUTOHSCROLL             = $00000080;
  {$EXTERNALSYM ECO_NOHIDESEL}
  ECO_NOHIDESEL               = $00000100;
  {$EXTERNALSYM ECO_READONLY}
  ECO_READONLY                = $00000800;
  {$EXTERNALSYM ECO_WANTRETURN}
  ECO_WANTRETURN              = $00001000;
  {$EXTERNALSYM ECO_SAVESEL}
  ECO_SAVESEL                 = $00008000;
  {$EXTERNALSYM ECO_SELECTIONBAR}
  ECO_SELECTIONBAR            = $01000000;
  {$EXTERNALSYM ECO_VERTICAL}
  ECO_VERTICAL                = $00400000;              { FE specific }

{ ECO operations }

  {$EXTERNALSYM ECOOP_SET}
  ECOOP_SET                                   = $0001;
  {$EXTERNALSYM ECOOP_OR}
  ECOOP_OR                                    = $0002;
  {$EXTERNALSYM ECOOP_AND}
  ECOOP_AND                                   = $0003;
  {$EXTERNALSYM ECOOP_XOR}
  ECOOP_XOR                                   = $0004;

{ new word break function actions }

  {$EXTERNALSYM WB_CLASSIFY}
  WB_CLASSIFY                 = 3;
  {$EXTERNALSYM WB_MOVEWORDLEFT}
  WB_MOVEWORDLEFT             = 4;
  {$EXTERNALSYM WB_MOVEWORDRIGHT}
  WB_MOVEWORDRIGHT            = 5;
  {$EXTERNALSYM WB_LEFTBREAK}
  WB_LEFTBREAK                = 6;
  {$EXTERNALSYM WB_RIGHTBREAK}
  WB_RIGHTBREAK               = 7;

{ Far East specific flags }

  {$EXTERNALSYM WB_MOVEWORDPREV}
  WB_MOVEWORDPREV             = 4;
  {$EXTERNALSYM WB_MOVEWORDNEXT}
  WB_MOVEWORDNEXT             = 5;
  {$EXTERNALSYM WB_PREVBREAK}
  WB_PREVBREAK                = 6;
  {$EXTERNALSYM WB_NEXTBREAK}
  WB_NEXTBREAK                = 7;

  {$EXTERNALSYM PC_FOLLOWING}
  PC_FOLLOWING                = 1;
  {$EXTERNALSYM PC_LEADING}
  PC_LEADING                  = 2;
  {$EXTERNALSYM PC_OVERFLOW}
  PC_OVERFLOW                 = 3;
  {$EXTERNALSYM PC_DELIMITER}
  PC_DELIMITER                = 4;
  {$EXTERNALSYM WBF_WORDWRAP}
  WBF_WORDWRAP                = $010;
  {$EXTERNALSYM WBF_WORDBREAK}
  WBF_WORDBREAK               = $020;
  {$EXTERNALSYM WBF_OVERFLOW}
  WBF_OVERFLOW                = $040;
  {$EXTERNALSYM WBF_LEVEL1}
  WBF_LEVEL1                  = $080;
  {$EXTERNALSYM WBF_LEVEL2}
  WBF_LEVEL2                  = $100;
  {$EXTERNALSYM WBF_CUSTOM}
  WBF_CUSTOM                  = $200;

{ Far East specific flags }

  {$EXTERNALSYM IMF_FORCENONE}
  IMF_FORCENONE               = $0001;
  {$EXTERNALSYM IMF_FORCEENABLE}
  IMF_FORCEENABLE             = $0002;
  {$EXTERNALSYM IMF_FORCEDISABLE}
  IMF_FORCEDISABLE            = $0004;
  {$EXTERNALSYM IMF_CLOSESTATUSWINDOW}
  IMF_CLOSESTATUSWINDOW       = $0008;
  {$EXTERNALSYM IMF_VERTICAL}
  IMF_VERTICAL                = $0020;
  {$EXTERNALSYM IMF_FORCEACTIVE}
  IMF_FORCEACTIVE             = $0040;
  {$EXTERNALSYM IMF_FORCEINACTIVE}
  IMF_FORCEINACTIVE           = $0080;
  {$EXTERNALSYM IMF_FORCEREMEMBER}
  IMF_FORCEREMEMBER           = $0100;
  {$EXTERNALSYM IMF_MULTIPLEEDIT}
  IMF_MULTIPLEEDIT            = $0400;

{ Word break flags (used with WB_CLASSIFY) }

  {$EXTERNALSYM WBF_CLASS}
  WBF_CLASS                   = $0F;
  {$EXTERNALSYM WBF_ISWHITE}
  WBF_ISWHITE                 = $10;
  {$EXTERNALSYM WBF_BREAKLINE}
  WBF_BREAKLINE               = $20;
  {$EXTERNALSYM WBF_BREAKAFTER}
  WBF_BREAKAFTER              = $40;

{ all character format measurements are in twips }

type
  CHARFORMATA = record
    cbSize: UINT;
    dwMask: Longint;
    dwEffects: Longint;
    yHeight: Longint;
    yOffset: Longint;
    crTextColor: TColorRef;
    bCharSet: Byte;
    bPitchAndFamily: Byte;
    szFaceName: array[0..LF_FACESIZE - 1] of AnsiChar;
  end;
  {$EXTERNALSYM CHARFORMATA}
  CHARFORMATW = record
    cbSize: UINT;
    dwMask: Longint;
    dwEffects: Longint;
    yHeight: Longint;
    yOffset: Longint;
    crTextColor: TColorRef;
    bCharSet: Byte;
    bPitchAndFamily: Byte;
    szFaceName: array[0..LF_FACESIZE - 1] of WideChar;
  end;
  {$EXTERNALSYM CHARFORMATW}
  CHARFORMAT = CHARFORMATW;
  TCharFormatA = CHARFORMATA;
  TCharFormatW = CHARFORMATW;
  TCharFormat = TCharFormatW;

{ CHARFORMAT masks }

const
  {$EXTERNALSYM CFM_BOLD}
  CFM_BOLD            = $00000001;
  {$EXTERNALSYM CFM_ITALIC}
  CFM_ITALIC          = $00000002;
  {$EXTERNALSYM CFM_UNDERLINE}
  CFM_UNDERLINE       = $00000004;
  {$EXTERNALSYM CFM_STRIKEOUT}
  CFM_STRIKEOUT       = $00000008;
  {$EXTERNALSYM CFM_PROTECTED}
  CFM_PROTECTED       = $00000010;
  {$EXTERNALSYM CFM_LINK}
  CFM_LINK            = $00000020;              { Exchange hyperlink extension }
  {$EXTERNALSYM CFM_SIZE}
  CFM_SIZE            = $80000000;
  {$EXTERNALSYM CFM_COLOR}
  CFM_COLOR           = $40000000;
  {$EXTERNALSYM CFM_FACE}
  CFM_FACE            = $20000000;
  {$EXTERNALSYM CFM_OFFSET}
  CFM_OFFSET          = $10000000;
  {$EXTERNALSYM CFM_CHARSET}
  CFM_CHARSET         = $08000000;

{ CHARFORMAT effects }

  {$EXTERNALSYM CFE_BOLD}
  CFE_BOLD            = $0001;
  {$EXTERNALSYM CFE_ITALIC}
  CFE_ITALIC          = $0002;
  {$EXTERNALSYM CFE_UNDERLINE}
  CFE_UNDERLINE       = $0004;
  {$EXTERNALSYM CFE_STRIKEOUT}
  CFE_STRIKEOUT       = $0008;
  {$EXTERNALSYM CFE_PROTECTED}
  CFE_PROTECTED       = $0010;
  {$EXTERNALSYM CFE_LINK}
  CFE_LINK            = $0020;
  {$EXTERNALSYM CFE_AUTOCOLOR}
  CFE_AUTOCOLOR       = $40000000;  { NOTE: this corresponds to CFM_COLOR, }
                                    { which controls it }
  {$EXTERNALSYM yHeightCharPtsMost}
  yHeightCharPtsMost  = 1638;

{ EM_SETCHARFORMAT wParam masks }

  {$EXTERNALSYM SCF_SELECTION}
  SCF_SELECTION       = $0001;
  {$EXTERNALSYM SCF_WORD}
  SCF_WORD            = $0002;
  {$EXTERNALSYM SCF_DEFAULT}
  SCF_DEFAULT         = $0000;          { set the default charformat or paraformat }
  {$EXTERNALSYM SCF_ALL}
  SCF_ALL             = $0004;          { not valid with SCF_SELECTION or SCF_WORD }
  {$EXTERNALSYM SCF_USEUIRULES}
  SCF_USEUIRULES      = $0008;          { modifier for SCF_SELECTION; says that }
                                        { the format came from a toolbar, etc. and }
                                        { therefore UI formatting rules should be }
                                        { used instead of strictly formatting the }
                                        { selection. }

type
  CHARRANGE = record
    cpMin: Longint;
    cpMax: LongInt;
  end;
  {$EXTERNALSYM CHARRANGE}
  TCharRange = CHARRANGE;

  TEXTRANGEA = record
    chrg: TCharRange;
    lpstrText: LPSTR;
  end;
  {$EXTERNALSYM TEXTRANGEA}
  TTextRangeA = TEXTRANGEA;
  TEXTRANGEW = record
    chrg: TCharRange;
    lpstrText: LPWSTR;
  end;
  {$EXTERNALSYM TEXTRANGEW}
  TTextRangeW = TEXTRANGEW;
  TEXTRANGE = TEXTRANGEW;

type
  TEditStreamCallBack = function (dwCookie: DWORD_PTR; pbBuff: PByte;
    cb: Longint; var pcb: Longint): Longint; stdcall;

  EDITSTREAM = record
    dwCookie: DWORD_PTR;
    dwError: Longint;
    pfnCallback: TEditStreamCallBack;
  end;
  TEditStream = EDITSTREAM;
  {$EXTERNALSYM EDITSTREAM}

{ stream formats }

const
  {$EXTERNALSYM SF_TEXT}
  SF_TEXT             = $0001;
  {$EXTERNALSYM SF_RTF}
  SF_RTF              = $0002;
  {$EXTERNALSYM SF_RTFNOOBJS}
  SF_RTFNOOBJS        = $0003;          { outbound only }
  {$EXTERNALSYM SF_TEXTIZED}
  SF_TEXTIZED         = $0004;          { outbound only }
  {$EXTERNALSYM SF_UNICODE}
  SF_UNICODE          = $0010;          { Unicode file of some kind }

{ Flag telling stream operations to operate on the selection only }
{ EM_STREAMIN will replace the current selection }
{ EM_STREAMOUT will stream out the current selection }

  {$EXTERNALSYM SFF_SELECTION}
  SFF_SELECTION       = $8000;

{ Flag telling stream operations to operate on the common RTF keyword only }
{ EM_STREAMIN will accept the only common RTF keyword }
{ EM_STREAMOUT will stream out the only common RTF keyword }

  {$EXTERNALSYM SFF_PLAINRTF}
  SFF_PLAINRTF        = $4000;

{ EM_FINDTEXT flags (removed in 3.0 SDK - leave in!) }

  FT_MATCHCASE = 4 deprecated 'Use CommDlg.FR_MATCHCASE';
  FT_WHOLEWORD = 2 deprecated 'Use CommDlg.FR_WHOLEWORD';

type
  FINDTEXTA = record
    chrg: TCharRange;
    lpstrText: LPSTR;
  end;
  {$EXTERNALSYM FINDTEXTA}
  FINDTEXTW = record
    chrg: TCharRange;
    lpstrText: LPWSTR;
  end;
  {$EXTERNALSYM FINDTEXTW}
  FINDTEXT = FINDTEXTW;
  {$EXTERNALSYM FINDTEXT}
  TFindTextA = FINDTEXTA;
  TFindTextW = FINDTEXTW;
  TFindText = TFindTextW;

  FINDTEXTEXA = record
    chrg: TCharRange;
    lpstrText: LPSTR;
    chrgText: TCharRange;
  end;
  {$EXTERNALSYM FINDTEXTEXA}
  FINDTEXTEXW = record
    chrg: TCharRange;
    lpstrText: LPWSTR;
    chrgText: TCharRange;
  end;
  {$EXTERNALSYM FINDTEXTEXW}
  FINDTEXTEX = FINDTEXTEXW;
  {$EXTERNALSYM FINDTEXTEX}
  TFindTextExA = FINDTEXTEXA;
  TFindTextExW = FINDTEXTEXW;
  TFindTextEx = TFindTextExW;

  FORMATRANGE = record
    hdc: HDC;
    hdcTarget: HDC;
    rc: TRect;
    rcPage: TRect;
    chrg: TCharRange;
  end;
  {$EXTERNALSYM FORMATRANGE}
  TFormatRange = FORMATRANGE;

{ all paragraph measurements are in twips }

const
  {$EXTERNALSYM MAX_TAB_STOPS}
  MAX_TAB_STOPS     = 32;
  {$EXTERNALSYM lDefaultTab}
  lDefaultTab     = 720;

type
  PARAFORMAT = record
    cbSize: UINT;
    dwMask: DWORD;
    wNumbering: Word;
    wReserved: Word;
    dxStartIndent: Longint;
    dxRightIndent: Longint;
    dxOffset: Longint;
    wAlignment: Word;
    cTabCount: Smallint;
    rgxTabs: array [0..MAX_TAB_STOPS - 1] of Longint;
  end;
  {$EXTERNALSYM PARAFORMAT}
  TParaFormat = PARAFORMAT;

{ PARAFORMAT mask values }

const
  {$EXTERNALSYM PFM_STARTINDENT}
  PFM_STARTINDENT                     = $00000001;
  {$EXTERNALSYM PFM_RIGHTINDENT}
  PFM_RIGHTINDENT                     = $00000002;
  {$EXTERNALSYM PFM_OFFSET}
  PFM_OFFSET                          = $00000004;
  {$EXTERNALSYM PFM_ALIGNMENT}
  PFM_ALIGNMENT                       = $00000008;
  {$EXTERNALSYM PFM_TABSTOPS}
  PFM_TABSTOPS                        = $00000010;
  {$EXTERNALSYM PFM_NUMBERING}
  PFM_NUMBERING                       = $00000020;
  {$EXTERNALSYM PFM_OFFSETINDENT}
  PFM_OFFSETINDENT                    = $80000000;

{ PARAFORMAT numbering options }

  {$EXTERNALSYM PFN_BULLET}
  PFN_BULLET                  = $0001;

{ PARAFORMAT alignment options }

  {$EXTERNALSYM PFA_LEFT}
  PFA_LEFT            = $0001;
  {$EXTERNALSYM PFA_RIGHT}
  PFA_RIGHT           = $0002;
  {$EXTERNALSYM PFA_CENTER}
  PFA_CENTER          = $0003;

type
  _CHARFORMAT2A = record
    cbSize: UINT;
    dwMask: DWORD;
    dwEffects: DWORD;
    yHeight: Longint;
    yOffset: Longint;
    crTextColor: TColorRef;
    bCharSet: Byte;
    bPitchAndFamily: Byte;
    szFaceName: array[0..LF_FACESIZE - 1] of AnsiChar;
    wWeight: Word;                   { Font weight (LOGFONT value) }
    sSpacing: Smallint;              { Amount to space between letters }
    crBackColor: TColorRef;          { Background color }
    lid: LCID;                       { Locale ID }
    dwReserved: DWORD;               { Reserved. Must be 0 }
    sStyle: Smallint;                { Style handle }
    wKerning: Word;                  { Twip size above which to kern char pair }
    bUnderlineType: Byte;            { Underline type }
    bAnimation: Byte;                { Animated text like marching ants }
    bRevAuthor: Byte;                { Revision author index }
    bReserved1: Byte;
  end;
  _CHARFORMAT2W = record
    cbSize: UINT;
    dwMask: DWORD;
    dwEffects: DWORD;
    yHeight: Longint;
    yOffset: Longint;
    crTextColor: TColorRef;
    bCharSet: Byte;
    bPitchAndFamily: Byte;
    szFaceName: array[0..LF_FACESIZE - 1] of WideChar;
    wWeight: Word;                   { Font weight (LOGFONT value) }
    sSpacing: Smallint;              { Amount to space between letters }
    crBackColor: TColorRef;          { Background color }
    lid: LCID;                       { Locale ID }
    dwReserved: DWORD;               { Reserved. Must be 0 }
    sStyle: Smallint;                { Style handle }
    wKerning: Word;                  { Twip size above which to kern char pair }
    bUnderlineType: Byte;            { Underline type }
    bAnimation: Byte;                { Animated text like marching ants }
    bRevAuthor: Byte;                { Revision author index }
    bReserved1: Byte;
  end;
  CHARFORMAT2A = _CHARFORMAT2A;
  {$EXTERNALSYM CHARFORMAT2A}
  CHARFORMAT2W = _CHARFORMAT2W;
  {$EXTERNALSYM CHARFORMAT2W}
  CHARFORMAT2 = CHARFORMAT2W;
  {$EXTERNALSYM CHARFORMAT2}
  TCharFormat2A = _CHARFORMAT2A;
  TCharFormat2W = _CHARFORMAT2W;
  TCharFormat2 = TCharFormat2W;

{ CHARFORMAT and PARAFORMAT "ALL" masks
  CFM_COLOR mirrors CFE_AUTOCOLOR, a little hack to easily deal with autocolor }
const
  {$EXTERNALSYM CFM_EFFECTS}
  CFM_EFFECTS = CFM_BOLD or CFM_ITALIC or CFM_UNDERLINE or CFM_COLOR or
    CFM_STRIKEOUT or CFE_PROTECTED or CFM_LINK;
  {$EXTERNALSYM CFM_ALL}
  CFM_ALL = CFM_EFFECTS or CFM_SIZE or CFM_FACE or CFM_OFFSET or CFM_CHARSET;

{ New masks and effects -- a parenthesized asterisk indicates that
   the data is stored by RichEdit2.0, but not displayed }

  {$EXTERNALSYM CFM_SMALLCAPS}
  CFM_SMALLCAPS               = $0040;                  { (*) }
  {$EXTERNALSYM CFM_ALLCAPS}
  CFM_ALLCAPS                 = $0080;                  { (*) }
  {$EXTERNALSYM CFM_HIDDEN}
  CFM_HIDDEN                  = $0100;                  { (*) }
  {$EXTERNALSYM CFM_OUTLINE}
  CFM_OUTLINE                 = $0200;                  { (*) }
  {$EXTERNALSYM CFM_SHADOW}
  CFM_SHADOW                  = $0400;                  { (*) }
  {$EXTERNALSYM CFM_EMBOSS}
  CFM_EMBOSS                  = $0800;                  { (*) }
  {$EXTERNALSYM CFM_IMPRINT}
  CFM_IMPRINT                 = $1000;                  { (*) }
  {$EXTERNALSYM CFM_DISABLED}
  CFM_DISABLED                = $2000;
  {$EXTERNALSYM CFM_REVISED}
  CFM_REVISED                 = $4000;

  {$EXTERNALSYM CFM_BACKCOLOR}
  CFM_BACKCOLOR               = $04000000;
  {$EXTERNALSYM CFM_COOKIE}
  CFM_COOKIE                  = $01000000;
  {$EXTERNALSYM CFM_LCID}
  CFM_LCID                    = $02000000;
  {$EXTERNALSYM CFM_UNDERLINETYPE}
  CFM_UNDERLINETYPE           = $00800000;              { (*) }
  {$EXTERNALSYM CFM_WEIGHT}
  CFM_WEIGHT                  = $00400000;
  {$EXTERNALSYM CFM_SPACING}
  CFM_SPACING                 = $00200000;              { (*) }
  {$EXTERNALSYM CFM_KERNING}
  CFM_KERNING                 = $00100000;              { (*) }
  {$EXTERNALSYM CFM_STYLE}
  CFM_STYLE                   = $00080000;              { (*) }
  {$EXTERNALSYM CFM_ANIMATION}
  CFM_ANIMATION               = $00040000;              { (*) }
  {$EXTERNALSYM CFM_REVAUTHOR}
  CFM_REVAUTHOR               = $00008000;

  {$EXTERNALSYM CFE_SUBSCRIPT}
  CFE_SUBSCRIPT               = $00010000;              { Superscript and subscript are }
  {$EXTERNALSYM CFE_SUPERSCRIPT}
  CFE_SUPERSCRIPT             = $00020000;              {  mutually exclusive }

  {$EXTERNALSYM CFM_SUBSCRIPT}
  CFM_SUBSCRIPT               = CFE_SUBSCRIPT or CFE_SUPERSCRIPT;
  {$EXTERNALSYM CFM_SUPERSCRIPT}
  CFM_SUPERSCRIPT             = CFM_SUBSCRIPT;

  {$EXTERNALSYM CFM_EFFECTS2}
  CFM_EFFECTS2 = CFM_EFFECTS or CFM_DISABLED or CFM_SMALLCAPS or CFM_ALLCAPS or
    CFM_HIDDEN  or CFM_OUTLINE or CFM_SHADOW or CFM_EMBOSS or
    CFM_IMPRINT or CFM_DISABLED or CFM_REVISED or
    CFM_SUBSCRIPT or CFM_SUPERSCRIPT or CFM_BACKCOLOR;

  {$EXTERNALSYM CFM_ALL2}
  CFM_ALL2 = CFM_ALL or CFM_EFFECTS2 or CFM_BACKCOLOR or CFM_LCID or
    CFM_UNDERLINETYPE or CFM_WEIGHT or CFM_REVAUTHOR or
    CFM_SPACING or CFM_KERNING or CFM_STYLE or CFM_ANIMATION or
    CFM_COOKIE;

  {$EXTERNALSYM CFE_SMALLCAPS}
  CFE_SMALLCAPS               = CFM_SMALLCAPS;
  {$EXTERNALSYM CFE_ALLCAPS}
  CFE_ALLCAPS                 = CFM_ALLCAPS;
  {$EXTERNALSYM CFE_HIDDEN}
  CFE_HIDDEN                  = CFM_HIDDEN;
  {$EXTERNALSYM CFE_OUTLINE}
  CFE_OUTLINE                 = CFM_OUTLINE;
  {$EXTERNALSYM CFE_SHADOW}
  CFE_SHADOW                  = CFM_SHADOW;
  {$EXTERNALSYM CFE_EMBOSS}
  CFE_EMBOSS                  = CFM_EMBOSS;
  {$EXTERNALSYM CFE_IMPRINT}
  CFE_IMPRINT                 = CFM_IMPRINT;
  {$EXTERNALSYM CFE_DISABLED}
  CFE_DISABLED                = CFM_DISABLED;
  {$EXTERNALSYM CFE_REVISED}
  CFE_REVISED                 = CFM_REVISED;

{ NOTE: CFE_AUTOCOLOR and CFE_AUTOBACKCOLOR correspond to CFM_COLOR and
   CFM_BACKCOLOR, respectively, which control them }

  {$EXTERNALSYM CFE_AUTOBACKCOLOR}
  CFE_AUTOBACKCOLOR           = CFM_BACKCOLOR;

{ Underline types }

  {$EXTERNALSYM CFU_CF1UNDERLINE}
  CFU_CF1UNDERLINE            = $FF;    { map charformat's bit underline to CF2. }
  {$EXTERNALSYM CFU_INVERT}
  CFU_INVERT                  = $FE;    { For IME composition fake a selection. }
  {$EXTERNALSYM CFU_UNDERLINEDOTTED}
  CFU_UNDERLINEDOTTED         = $4;             { (*) displayed as ordinary underline }
  {$EXTERNALSYM CFU_UNDERLINEDOUBLE}
  CFU_UNDERLINEDOUBLE         = $3;             { (*) displayed as ordinary underline }
  {$EXTERNALSYM CFU_UNDERLINEWORD}
  CFU_UNDERLINEWORD           = $2;             { (*) displayed as ordinary underline }
  {$EXTERNALSYM CFU_UNDERLINE}
  CFU_UNDERLINE               = $1;
  {$EXTERNALSYM CFU_UNDERLINENONE}
  CFU_UNDERLINENONE           = 0;

type
  PARAFORMAT2 = record
    cbSize: UINT;
    dwMask: DWORD;
    wNumbering: Word;
    wReserved: Word;
    dxStartIndent: Longint;
    dxRightIndent: Longint;
    dxOffset: Longint;
    wAlignment: Word;
    cTabCount: Smallint;
    rgxTabs: array [0..MAX_TAB_STOPS - 1] of Longint;
    dySpaceBefore: Longint;     { Vertical spacing before para }
    dySpaceAfter: Longint;      { Vertical spacing after para }
    dyLineSpacing: Longint;     { Line spacing depending on Rule }
    sStyle: Smallint;           { Style handle }
    bLineSpacingRule: Byte;     { Rule for line spacing (see tom.doc) }
    bOutlineLevel: Byte;        { Outline Level }
    wShadingWeight: Word;       { Shading in hundredths of a per cent }
    wShadingStyle: Word;        { Nibble 0: style, 1: cfpat, 2: cbpat }
    wNumberingStart: Word;      { Starting value for numbering }
    wNumberingStyle: Word;      { Alignment, roman/arabic, (), ), ., etc. }
    wNumberingTab: Word;        { Space bet 1st indent and 1st-line text }
    wBorderSpace: Word;         { Space between border and text (twips) }
    wBorderWidth: Word;         { Border pen width (twips) }
    wBorders: Word;             { Byte 0: bits specify which borders }
                                { Nibble 2: border style, 3: color index }
  end;
  TParaFormat2 = PARAFORMAT2;
  {$EXTERNALSYM PARAFORMAT2}

{ PARAFORMAT 2.0 masks and effects }
const
  {$EXTERNALSYM PFM_SPACEBEFORE}
  PFM_SPACEBEFORE                     = $00000040;
  {$EXTERNALSYM PFM_SPACEAFTER}
  PFM_SPACEAFTER                      = $00000080;
  {$EXTERNALSYM PFM_LINESPACING}
  PFM_LINESPACING                     = $00000100;
  {$EXTERNALSYM PFM_STYLE}
  PFM_STYLE                           = $00000400;
  {$EXTERNALSYM PFM_BORDER}
  PFM_BORDER                          = $00000800;      { (*) }
  {$EXTERNALSYM PFM_SHADING}
  PFM_SHADING                         = $00001000;      { (*) }
  {$EXTERNALSYM PFM_NUMBERINGSTYLE}
  PFM_NUMBERINGSTYLE                  = $00002000;      { (*) }
  {$EXTERNALSYM PFM_NUMBERINGTAB}
  PFM_NUMBERINGTAB                    = $00004000;      { (*) }
  {$EXTERNALSYM PFM_NUMBERINGSTART}
  PFM_NUMBERINGSTART                  = $00008000;      { (*) }

  {$EXTERNALSYM PFM_RTLPARA}
  PFM_RTLPARA                         = $00010000;
  {$EXTERNALSYM PFM_KEEP}
  PFM_KEEP                            = $00020000;      { (*) }
  {$EXTERNALSYM PFM_KEEPNEXT}
  PFM_KEEPNEXT                        = $00040000;      { (*) }
  {$EXTERNALSYM PFM_PAGEBREAKBEFORE}
  PFM_PAGEBREAKBEFORE                 = $00080000;      { (*) }
  {$EXTERNALSYM PFM_NOLINENUMBER}
  PFM_NOLINENUMBER                    = $00100000;      { (*) }
  {$EXTERNALSYM PFM_NOWIDOWCONTROL}
  PFM_NOWIDOWCONTROL                  = $00200000;      { (*) }
  {$EXTERNALSYM PFM_DONOTHYPHEN}
  PFM_DONOTHYPHEN                     = $00400000;      { (*) }
  {$EXTERNALSYM PFM_SIDEBYSIDE}
  PFM_SIDEBYSIDE                      = $00800000;      { (*) }

  {$EXTERNALSYM PFM_TABLE}
  PFM_TABLE                           = $40000000;      { (*) }

  {$EXTERNALSYM PFM_TEXTWRAPPINGBREAK}
  PFM_TEXTWRAPPINGBREAK               = $20000000;

  {$EXTERNALSYM PFM_TABLEROWDELIMITER}
  PFM_TABLEROWDELIMITER               = $10000000;

  // The following three properties are read only
  {$EXTERNALSYM PFM_COLLAPSED}
  PFM_COLLAPSED                       = $01000000;
  {$EXTERNALSYM PFM_OUTLINELEVEL}
  PFM_OUTLINELEVEL                    = $02000000;
  {$EXTERNALSYM PFM_BOX}
  PFM_BOX                             = $04000000;
  //{$EXTERNALSYM PFM_RESERVED2}
  //PFM_RESERVED2                       = $08000000;

  {$EXTERNALSYM PFM_ALL}
  PFM_ALL = PFM_STARTINDENT or PFM_RIGHTINDENT or PFM_OFFSET or
    PFM_ALIGNMENT or PFM_TABSTOPS or PFM_NUMBERING or
    PFM_OFFSETINDENT or PFM_RTLPARA;

{ Note: PARAFORMAT has no effects }

  {$EXTERNALSYM PFM_EFFECTS}
  PFM_EFFECTS = PFM_RTLPARA or PFM_KEEP or PFM_KEEPNEXT or PFM_TABLE or
    PFM_PAGEBREAKBEFORE or PFM_NOLINENUMBER or
    PFM_NOWIDOWCONTROL or PFM_DONOTHYPHEN or PFM_SIDEBYSIDE or
    PFM_TABLE or PFM_TABLEROWDELIMITER;

  {$EXTERNALSYM PFM_ALL2}
  PFM_ALL2 = PFM_ALL or PFM_EFFECTS or PFM_SPACEBEFORE or PFM_SPACEAFTER or
    PFM_LINESPACING or PFM_STYLE or PFM_SHADING or PFM_BORDER or
    PFM_NUMBERINGTAB or PFM_NUMBERINGSTART or PFM_NUMBERINGSTYLE;

  {$EXTERNALSYM PFE_RTLPARA}
  PFE_RTLPARA                         = PFM_RTLPARA         shr 16;
  {$EXTERNALSYM PFE_KEEP}
  PFE_KEEP                            = PFM_KEEP            shr 16; { (*) }
  {$EXTERNALSYM PFE_KEEPNEXT}
  PFE_KEEPNEXT                        = PFM_KEEPNEXT        shr 16; { (*) }
  {$EXTERNALSYM PFE_PAGEBREAKBEFORE}
  PFE_PAGEBREAKBEFORE                 = PFM_PAGEBREAKBEFORE shr 16; { (*) }
  {$EXTERNALSYM PFE_NOLINENUMBER}
  PFE_NOLINENUMBER                    = PFM_NOLINENUMBER    shr 16; { (*) }
  {$EXTERNALSYM PFE_NOWIDOWCONTROL}
  PFE_NOWIDOWCONTROL                  = PFM_NOWIDOWCONTROL  shr 16; { (*) }
  {$EXTERNALSYM PFE_DONOTHYPHEN}
  PFE_DONOTHYPHEN                     = PFM_DONOTHYPHEN     shr 16; { (*) }
  {$EXTERNALSYM PFE_SIDEBYSIDE}
  PFE_SIDEBYSIDE                      = PFM_SIDEBYSIDE      shr 16; { (*) }

  PFE_TABLEROW                        = $c000;          { These 3 options are mutually }
  PFE_TABLECELLEND                    = $8000;          { exclusive and each imply }
  PFE_TABLECELL                       = $4000;          { that para is part of a table }

{
 *  PARAFORMAT numbering options (values for wNumbering):
 *
 *    Numbering Type       Value  Meaning
 *    tomNoNumbering         0    Turn off paragraph numbering
 *    tomNumberAsLCLetter    1    a, b, c, ...
 *    tomNumberAsUCLetter    2    A, B, C, ...
 *    tomNumberAsLCRoman     3    i, ii, iii, ...
 *    tomNumberAsUCRoman     4    I, II, III, ...
 *    tomNumberAsSymbols     5    default is bullet
 *    tomNumberAsNumber      6    0, 1, 2, ...
 *    tomNumberAsSequence    7    tomNumberingStart is first Unicode to use
 *
 *  Other valid Unicode chars are Unicodes for bullets.
}

  {$EXTERNALSYM PFA_JUSTIFY}
  PFA_JUSTIFY                         = 4;      { New paragraph-alignment option 2.0 (*) }

{ notification structures }
type
  PMsgFilter = ^TMsgFilter;
  MSGFILTER = record
    nmhdr: TNMHdr;
    msg: UINT;
    wParam: WPARAM;
    lParam: LPARAM;
  end;
  {$EXTERNALSYM MSGFILTER}
  TMsgFilter = MSGFILTER;

  PReqSize = ^TReqSize;
  TReqSize = record
    nmhdr: TNMHdr;
    rc: TRect;
  end;

  PSelChange = ^TSelChange;
  SELCHANGE = record
    nmhdr: TNMHdr;
    chrg: TCharRange;
    seltyp: Word;
  end;
  {$EXTERNALSYM SELCHANGE}
  TSelChange = SELCHANGE;

const
  {$EXTERNALSYM SEL_EMPTY}
  SEL_EMPTY           = $0000;
  {$EXTERNALSYM SEL_TEXT}
  SEL_TEXT            = $0001;
  {$EXTERNALSYM SEL_OBJECT}
  SEL_OBJECT          = $0002;
  {$EXTERNALSYM SEL_MULTICHAR}
  SEL_MULTICHAR       = $0004;
  {$EXTERNALSYM SEL_MULTIOBJECT}
  SEL_MULTIOBJECT     = $0008;

{ used with IRichEditOleCallback::GetContextMenu, this flag will be
   passed as a "selection type".  It indicates that a context menu for
   a right-mouse drag drop should be generated.  The IOleObject parameter
   will really be the IDataObject for the drop
}
  {$EXTERNALSYM GCM_RIGHTMOUSEDROP}
  GCM_RIGHTMOUSEDROP      = $8000;

type
  TEndDropFiles = record
    nmhdr: TNMHdr;
    hDrop: THandle;
    cp: Longint;
    fProtected: Bool;
  end;

  PENProtected = ^TENProtected;
  ENPROTECTED = record
    nmhdr: TNMHdr;
    msg: UINT;
    wParam: WPARAM;
    lParam: LPARAM;
    chrg: TCharRange;
  end;
  {$EXTERNALSYM ENPROTECTED}
  TENProtected = ENPROTECTED;

  PENSaveClipboard = ^TENSaveClipboard;
  ENSAVECLIPBOARD = record
    nmhdr: TNMHdr;
    cObjectCount: Longint;
    cch: Longint;
  end;
  {$EXTERNALSYM ENSAVECLIPBOARD}
  TENSaveClipboard = ENSAVECLIPBOARD;

  ENOLEOPFAILED = record
    nmhdr: TNMHdr;
    iob: Longint;
    lOper: Longint;
    hr: HRESULT;
  end;
  {$EXTERNALSYM ENOLEOPFAILED}
  TENOleOpFailed = ENOLEOPFAILED;

const
  {$EXTERNALSYM OLEOP_DOVERB}
  OLEOP_DOVERB        = 1;

type
  OBJECTPOSITIONS = record
    nmhdr: TNMHdr;
    cObjectCount: Longint;
    pcpPositions: PLongint;
  end;
  {$EXTERNALSYM OBJECTPOSITIONS}
  TObjectPositions = OBJECTPOSITIONS;

  PENLink = ^TENLink;
  ENLINK = record
    nmhdr: TNMHdr;
    msg: UINT;
    wParam: WPARAM;
    lParam: LPARAM;
    chrg: TCharRange;
  end;
  {$EXTERNALSYM ENLINK}
  TENLink = ENLINK;

{ PenWin specific }
  ENCORRECTTEXT = record
    nmhdr: TNMHdr;
    chrg: TCharRange;
    seltyp: Word;
  end;
  {$EXTERNALSYM ENCORRECTTEXT}
  TENCorrectText = ENCORRECTTEXT;

{ Far East specific }
  PUNCTUATION = record
    iSize: UINT;
    szPunctuation: PAnsiChar;
  end;
  {$EXTERNALSYM PUNCTUATION}
  TPunctuation = PUNCTUATION;

{ Far East specific }
  COMPCOLOR = record
    crText: TColorRef;
    crBackground: TColorRef;
    dwEffects: Longint;
  end;
  {$EXTERNALSYM COMPCOLOR}
  TCompColor = COMPCOLOR;

{ clipboard formats - use as parameter to RegisterClipboardFormat }

const
  {$EXTERNALSYM CF_RTF}
  CF_RTF                 = 'Rich Text Format';
  {$EXTERNALSYM CF_RTFNOOBJS}
  CF_RTFNOOBJS           = 'Rich Text Format Without Objects';
  {$EXTERNALSYM CF_RETEXTOBJ}
  CF_RETEXTOBJ           = 'RichEdit Text and Objects';

type
  REPASTESPECIAL = record
    dwAspect: DWORD;
    dwParam: DWORD_PTR;
  end;
  {$EXTERNALSYM REPASTESPECIAL}
  TRepasteSpecial = REPASTESPECIAL;

{ UndoName info }

  UNDONAMEID = (UID_UNKNOWN, UID_TYPING, UID_DELETE, UID_DRAGDROP, UID_CUT,
    UID_PASTE);
  {$EXTERNALSYM UNDONAMEID}

{ flags for the GETEXTEX data structure }

const
  GT_DEFAULT                  = 0;
  {$EXTERNALSYM GT_DEFAULT}
  GT_USECRLF                  = 1;
  {$EXTERNALSYM GT_USECRLF}
  GT_SELECTION                = 2;
  {$EXTERNALSYM GT_SELECTION}
  GT_RAWTEXT                  = 4;
  {$EXTERNALSYM GT_RAWTEXT}
  GT_NOHIDDENTEXT             = 8;
  {$EXTERNALSYM GT_NOHIDDENTEXT}

{ EM_GETTEXTEX info; this struct is passed in the wparam of the message }

type
  GETTEXTEX = record
    cb: DWORD;                 { count of bytes in the string }
    flags: DWORD;              { flags (see the GT_XXX defines }
    codepage: UINT;            { code page for translation (CP_ACP for default, 1200 for Unicode }
    lpDefaultChar: LPCSTR;     { replacement for unmappable chars }
    lpUsedDefChar: PBOOL;      { pointer to flag set when def char used }
  end;
  {$EXTERNALSYM GETTEXTEX}
  TGetTextEx = GETTEXTEX;
{ flags for the GETTEXTLENGTHEX data structure }

const
  {$EXTERNALSYM GTL_DEFAULT}
  GTL_DEFAULT         = 0;      { do the default (return # of chars) }
  {$EXTERNALSYM GTL_USECRLF}
  GTL_USECRLF         = 1;      { compute answer using CRLFs for paragraphs }
  {$EXTERNALSYM GTL_PRECISE}
  GTL_PRECISE         = 2;      { compute a precise answer }
  {$EXTERNALSYM GTL_CLOSE}
  GTL_CLOSE           = 4;      { fast computation of a "close" answer }
  {$EXTERNALSYM GTL_NUMCHARS}
  GTL_NUMCHARS        = 8;      { return the number of characters }
  {$EXTERNALSYM GTL_NUMBYTES}
  GTL_NUMBYTES        = 16;     { return the number of _bytes_ }

{ EM_GETTEXTLENGTHEX info; this struct is passed in the wparam of the msg }

type
  GETTEXTLENGTHEX = record
    flags: DWORD;              { flags (see GTL_XXX defines)         }
    codepage: UINT;            { code page for translation (CP_ACP for default,
                                 1200 for Unicode            }
  end;
  {$EXTERNALSYM GETTEXTLENGTHEX}
  TGetTextLengthEx = GETTEXTLENGTHEX;

{ UNICODE embedding character }
const
  {$EXTERNALSYM WCH_EMBEDDING}
  WCH_EMBEDDING     = $FFFC;


// Utilities to simplify .NET/Win32 single code base
{$ALIGN 8}
type
  TWMNotifyRE = record { TWMNotify }
    Msg: Cardinal;
    IDCtrl: WPARAM;
    case Integer of
      0: (NMHdr: PNMHdr);
      1: (ENProtected: PENProtected);
      2: (ENSaveClipBoard: PENSaveClipBoard);
      3: (ENLink: PENLink);
      4: (ReqSize: PReqSize;
          Result: LRESULT);
  end;
{$ALIGN 4}

function SendEMGetTextExMessage(hWnd: HWND; Msg: UINT; const wParam: TGetTextEx;
  var lParam: string): LRESULT;

implementation

function SendEMGetTextExMessage(hWnd: HWND; Msg: UINT; const wParam: TGetTextEx;
  var lParam: string): LRESULT;
begin
  Result := SendMessage(hWnd, Msg, Winapi.Windows.WPARAM(@wParam), Winapi.Windows.LPARAM(PChar(lParam)));
  SetLength(lParam, Result);
end;

end.
