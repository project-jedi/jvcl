{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDsgn.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}


unit JvDsgn;

interface

uses {$IFDEF WIN32} Windows, {$ELSE} WinTypes, {$ENDIF} Classes, SysUtils,
 {$IFDEF Delphi6_Up}RTLConsts,DesignIntf, VCLEditors, DesignEditors,{$ELSE}DsgnIntf,{$ENDIF}
  Controls, Graphics, ExtCtrls, Menus, Forms;

type
{$IFNDEF Delphi4_Up}
  IDesigner = TDesigner;
  IFormDesigner = TFormDesigner;
{$ENDIF}


{ TJvFilenameProperty }

  TJvFilenameProperty = class(TStringProperty)
  protected
    function GetFilter: string; virtual;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

{ TJvDirNameProperty }

  TJvDirNameProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

{ TJvProgressControlProperty }

  TJvProgressControlProperty = class(TComponentProperty)
  private
    FProc: TGetStrProc;
    procedure CheckComponent(const AName: string);
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

{ TJvDBStringProperty }

  TJvDBStringProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); virtual;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

implementation

uses Consts, Dialogs, JvCConst, JvFileUtil, JvVCLUtils, JvPrgrss;

{ TJvFilenameProperty }

function TJvFilenameProperty.GetFilter: string;
begin
  Result := LoadStr(SDefaultFilter);
end;

procedure TJvFilenameProperty.Edit;
var
  FileOpen: TOpenDialog;
begin
  FileOpen := TOpenDialog.Create(Application);
  try
    FileOpen.Filename := GetValue;
    FileOpen.InitialDir := ExtractFilePath(FileOpen.Filename);
    if (ExtractFileName(FileOpen.Filename) = '') or not
      ValidFileName(ExtractFileName(FileOpen.Filename)) then
      FileOpen.Filename := '';
    FileOpen.Filter := GetFilter;
    FileOpen.Options := FileOpen.Options + [ofHideReadOnly];
    if FileOpen.Execute then SetValue(FileOpen.Filename);
  finally
    FileOpen.Free;
  end;
end;

function TJvFilenameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog {$IFDEF WIN32}, paRevertable {$ENDIF}];
end;

{ TJvDirNameProperty }

procedure TJvDirNameProperty.Edit;
var
  FolderName: string;
begin
  FolderName := GetValue;
  if BrowseDirectory(FolderName, ResStr(SSelectDirCap), 0) then
    SetValue(FolderName);
end;

function TJvDirNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog {$IFDEF WIN32}, paRevertable {$ENDIF}];
end;

{ TJvProgressControlProperty }

procedure TJvProgressControlProperty.CheckComponent(const AName: string);
var
  Component: TComponent;
begin
{$IFDEF WIN32}
  Component := Designer.GetComponent(AName);
{$ELSE}
  Component := Designer.Form.FindComponent(AName);
{$ENDIF}
  if (Component <> nil) and (Component is TControl) and
    SupportsProgressControl(TControl(Component)) and Assigned(FProc) then
    FProc(AName);
end;

procedure TJvProgressControlProperty.GetValues(Proc: TGetStrProc);
begin
  FProc := Proc;
  try
    inherited GetValues(CheckComponent);
  finally
    FProc := nil;
  end;
end;

{ TJvDBStringProperty }

function TJvDBStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TJvDBStringProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

procedure TJvDBStringProperty.GetValueList(List: TStrings);
begin
end;

end.
