{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCopyError.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvCopyError;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SetupApi, JvCommonDialogD, JvBaseDlg, JvTypes;

type
  TJvCopyError = class(TJvCommonDialogD)
  private
    FPathToSource: string;
    FPath: string;
    FDiskName: string;
    FStyle: TDiskStyles;
    FError: Integer;
    FTarget: string;
    FSource: string;
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
    property DiskName: string read FDiskName write FDiskName;
    property PathToSource: string read FPathToSource write FPathToSource;
    property NewPath: string read FPath write FPath;
    property SourceFile: string read FSource write FSource;
    property TargetFile: string read FTarget write FTarget;
    property W32ErrorCode: Integer read FError write FError default 0;
    property Style: TDiskStyles read FStyle write FStyle;
    function Execute: TDiskRes; override;
  end;

implementation

{**************************************************}

constructor TJvCopyError.Create(AOwner: TComponent);
begin
  inherited;
  // (rom) TODO missing?
  if AOwner is TWinControl then
  begin
    FDiskName := '';
    FPathToSource := '';
    FPath := '';
    FStyle := [];
    FSource := '';
    FTarget := '';
    FError := 0;
  end;
end;

{**************************************************}

function TJvCopyError.Execute: TDiskRes;
var
  Sty: DWORD;
  Required: DWORD;
  res: array[0..255] of Char;
begin
  // (rom) simplified/fixed
  Result := dsError;
  Sty := 0;
  if idfCheckFirst in FStyle then
    Sty := Sty or IDF_CHECKFIRST;
  if idfNoBeep in FStyle then
    Sty := Sty or IDF_NOBEEP;
  if idfNoBrowse in FStyle then
    Sty := Sty or IDF_NOBROWSE;
  if idfNoCompressed in FStyle then
    Sty := Sty or IDF_NOCOMPRESSED;
  if idfNoDetails in FStyle then
    Sty := Sty or IDF_NODETAILS;
  if idfNoForeground in FStyle then
    Sty := Sty or IDF_NOFOREGROUND;
  if idfNoSkip in FStyle then
    Sty := Sty or IDF_NOSKIP;
  if idfOemDisk in FStyle then
    Sty := Sty or IDF_OEMDISK;
  if idfWarnIfSkip in FStyle then
    Sty := Sty or IDF_WARNIFSKIP;

  if @FSetupCopyError <> nil then
  begin
    case FSetupCopyError(Fhandle, PChar(Ftitle), PChar(FDiskName),
      PChar(FPathToSource), PChar(FSource), PCHar(FTarget),
      FError, Sty, res, SizeOf(res), @Required) of
      DPROMPT_SUCCESS:
        begin
          FPath := StrPas(res);
          Result := dsSuccess;
        end;
      DPROMPT_CANCEL:
        Result := dsCancel;
      DPROMPT_SKIPFILE:
        Result := dsSkipfile;
    else
      Result := dsError;
    end;
  end;
end;

end.
