{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFileNameBox.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2002-06-03

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvFileNameBox;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Dialogs,
  JvTypes, JvCustomBox, JvWinDialogs;

type
  TJvFileNameDlgOptions = class(TPersistent)
  private
    FPlacesBar: Boolean;
    FCtl3d: Boolean;
    FHelpContext: Integer;
    FFilterIndex: Integer;
    FFilter: string;
    FTitle: string;
    FInitialDir: string;
    FDefaultExt: string;
    FOnChanged: TNotifyEvent;
    FOptions: TOpenOptions;
    procedure SetCtl3d(const Value: Boolean);
    procedure SetDefaultExt(const Value: string);
    procedure SetFilter(const Value: string);
    procedure SetFilterIndex(const Value: Integer);
    procedure SetHelpContext(const Value: Integer);
    procedure SetInitialDir(const Value: string);
    procedure SetOptions(const Value: TOpenOptions);
    procedure SetPlacesBar(const Value: Boolean);
    procedure SetTitle(const Value: string);
  protected
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    procedure Changed;
  public
    constructor Create;
  published
    property Title: string read FTitle write SetTitle;
    property Filter: string read FFilter write SetFilter;
    property InitialDir: string read FInitialDir write SetInitialDir;
    property Options: TOpenOptions read FOptions write SetOptions;
    property DefaultExt: string read FDefaultExt write SetDefaultExt;
    property FilterIndex: Integer read FFilterIndex write SetFilterIndex default 0;
    property HelpContext: Integer read FHelpContext write SetHelpContext default 0;
    property Ctl3d: Boolean read FCtl3d write SetCtl3d default True;
    property PlacesBar: Boolean read FPlacesBar write SetPlacesBar default True;
  end;

  TJvFileNameBox = class(TJvCustomBox)
  private
    FChoose: TJvOpenDialog2000;
    FOnOpened: TOnOpened;
    FOnOpenCanceled: TOnOpenCanceled;
    FDlgOptions: TJvFileNameDlgOptions;
    function GetFileName: TFileName;
    procedure SetFileName(Value: TFileName);
    function GetFiles: TStringList;
  protected
    procedure BtnClick(Sender: TObject); override;
    procedure DlgOptionsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DialogOptions: TJvFileNameDlgOptions read FDlgOptions write FDlgOptions;
    property FileName: TFileName read GetFileName write SetFileName;
    property Files: TStringList read GetFiles;

    property OnOpened: TOnOpened read FOnOpened write FOnOpened;
    property OnOpenCanceld: TOnOpenCanceled read FOnOpenCanceled write FOnOpenCanceled;
  end;

implementation

///////////////////////////////////////////////////////////
// TJvFileNameBox
///////////////////////////////////////////////////////////

constructor TJvFileNameBox.Create(AOwner: TComponent);
begin
  inherited;
  Button.Glyph.LoadFromResourceName(HInstance, 'FILE');
  FChoose := TJvOpenDialog2000.Create(Self);
  FDlgOptions := TJvFileNameDlgOptions.Create;
  FDlgOptions.OnChanged := DlgOptionsChanged;
end;

{*****************************************************}

procedure TJvFileNameBox.BtnClick(Sender: TObject);
begin
  try
    //Reselecting last entered filename
    FChoose.FileName := ExtractFileName(Edit.Text);
  except
  end;
  if FChoose.Execute then
  begin
    if ofAllowMultiSelect in FChoose.Options then
      Edit.Text := FChoose.Files.CommaText
    else
      Edit.Text := FChoose.FileName;
    if Assigned(FOnOpened) then
      FOnOpened(Self, Edit.Text);
  end
  else if Assigned(FOnOpenCanceled) then
    FOnOpenCanceled(Self);
end;

{*****************************************************}

destructor TJvFileNameBox.Destroy;
begin
  FChoose.Free;
  FDlgOptions.Free;
  inherited;
end;

{*****************************************************}

procedure TJvFileNameBox.DlgOptionsChanged(Sender: TObject);
begin
//  FChoose.OptionsEx.PlacesBar := FDlgOptions.PlacesBar;
  FChoose.Title := FDlgOptions.Title;
  FChoose.Filter := FDlgOptions.Filter;
  FChoose.InitialDir := FDlgOptions.InitialDir;
  FChoose.Options := FDlgOptions.Options;
  FChoose.DefaultExt := FDlgOptions.DefaultExt;
  FChoose.FilterIndex := FDlgOptions.FilterIndex;
  FChoose.HelpContext := FDlgOptions.HelpContext;
  FChoose.Ctl3d := FDlgOptions.Ctl3d;
end;

{*****************************************************}

function TJvFileNameBox.GetFileName: TFileName;
begin
  Result := Edit.text;
end;

{*****************************************************}

function TJvFileNameBox.GetFiles: TStringList;
begin
  Result := TStringList.Create;
  with Result do
    CommaText := Edit.Text;
end;

{*****************************************************}

procedure TJvFileNameBox.SetFileName(Value: TFileName);
begin
  Edit.Text := Value;
end;

///////////////////////////////////////////////////////////
// TJvFileNameDlgOptions
///////////////////////////////////////////////////////////

procedure TJvFileNameDlgOptions.Changed;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

{*****************************************************}

constructor TJvFileNameDlgOptions.Create;
begin
  FOptions := [ofHideReadOnly, ofEnableSizing];
  FFilterIndex := 0;
  FHelpContext := 0;
  FCtl3d := True;
  FPlacesBar := True;
end;

{*****************************************************}

procedure TJvFileNameDlgOptions.SetCtl3d(const Value: Boolean);
begin
  FCtl3d := Value;
  Changed;
end;

{*****************************************************}

procedure TJvFileNameDlgOptions.SetDefaultExt(const Value: string);
begin
  FDefaultExt := Value;
  Changed;
end;

{*****************************************************}

procedure TJvFileNameDlgOptions.SetFilter(const Value: string);
begin
  FFilter := Value;
  Changed;
end;

{*****************************************************}

procedure TJvFileNameDlgOptions.SetFilterIndex(const Value: Integer);
begin
  FFilterIndex := Value;
  Changed;
end;

{*****************************************************}

procedure TJvFileNameDlgOptions.SetHelpContext(const Value: Integer);
begin
  FHelpContext := Value;
  Changed;
end;

{*****************************************************}

procedure TJvFileNameDlgOptions.SetInitialDir(const Value: string);
begin
  FInitialDir := Value;
  Changed;
end;

{*****************************************************}

procedure TJvFileNameDlgOptions.SetOptions(const Value: TOpenOptions);
begin
  FOptions := Value;
  Changed;
end;

{*****************************************************}

procedure TJvFileNameDlgOptions.SetPlacesBar(const Value: Boolean);
begin
  FPlacesBar := Value;
  Changed;
end;

{*****************************************************}

procedure TJvFileNameDlgOptions.SetTitle(const Value: string);
begin
  FTitle := Value;
  Changed;
end;

end.
