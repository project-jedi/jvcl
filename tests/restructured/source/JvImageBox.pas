{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvImageBox.PAS, released on 2001-02-28.

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

unit JvImageBox;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Jpeg, Dialogs, ExtDlgs,
  JvTypes, JvCustomBox;

type
  TJvPictureDlgOptions = class(TPersistent)
  private
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
  end;

  TJvImageBox = class(TJvCustomBox)
  private
    FChoose: TOpenPictureDialog;
    FFiles: TStringList;
    FOnOpened: TOnOpened;
    FOnOpenCanceled: TOnOpenCanceled;
    FDlgOptions: TJvPictureDlgOptions;
    function GetFileName: TFileName;
    procedure SetFileName(Value: TFileName);
  protected
    procedure BtnClick(Sender: TObject); override;
    procedure DlgOptionsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property FileName: TFileName read GetFileName write SetFileName;
    property Files: TStringList read FFiles;
    property DialogOptions: TJvPictureDlgOptions read FDlgOptions write FDlgOptions;

    property OnOpened: TOnOpened read FOnOpened write FOnOpened;
    property OnOpenCanceled: TOnOpenCanceled read FOnOpenCanceled write FOnOpenCanceled;
  end;

implementation

///////////////////////////////////////////////////////////
// TJvImageBox
///////////////////////////////////////////////////////////

constructor TJvImageBox.Create(AOwner: TComponent);
begin
  inherited;
  FFiles := TStringList.Create;
  Button.Glyph.LoadFromResourceName(HInstance, 'PICT');
  FChoose := TOpenPictureDialog.Create(Self);
  FDlgOptions := TJvPictureDlgOptions.Create;
  FDlgOptions.OnChanged := DlgOptionsChanged;
end;

{*****************************************************}

destructor TJvImageBox.Destroy;
begin
  FFiles.Free;
  FChoose.Free;
  FDlgOptions.Free;
  inherited;
end;

{*****************************************************}

procedure TJvImageBox.DlgOptionsChanged(Sender: TObject);
begin
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

procedure TJvImageBox.SetFileName(Value: TFileName);
begin
  Edit.Text := Value;
end;

{*****************************************************}

function TJvImageBox.GetFileName: TFileName;
begin
  Result := Edit.Text;
end;

{*****************************************************}

procedure TJvImageBox.BtnClick(Sender: TObject);
begin
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

///////////////////////////////////////////////////////////
// TJvPictureDlgOptions
///////////////////////////////////////////////////////////

procedure TJvPictureDlgOptions.Changed;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

{*****************************************************}

constructor TJvPictureDlgOptions.Create;
begin
  FOptions := [ofHideReadOnly, ofEnableSizing];
  FFilterIndex := 0;
  FHelpContext := 0;
  FCtl3d := True;
end;

{*****************************************************}

procedure TJvPictureDlgOptions.SetCtl3d(const Value: Boolean);
begin
  FCtl3d := Value;
  Changed;
end;

{*****************************************************}

procedure TJvPictureDlgOptions.SetDefaultExt(const Value: string);
begin
  FDefaultExt := Value;
  Changed;
end;

{*****************************************************}

procedure TJvPictureDlgOptions.SetFilter(const Value: string);
begin
  FFilter := Value;
  Changed;
end;

{*****************************************************}

procedure TJvPictureDlgOptions.SetFilterIndex(const Value: Integer);
begin
  FFilterIndex := Value;
  Changed;
end;

{*****************************************************}

procedure TJvPictureDlgOptions.SetHelpContext(const Value: Integer);
begin
  FHelpContext := Value;
  Changed;
end;

{*****************************************************}

procedure TJvPictureDlgOptions.SetInitialDir(const Value: string);
begin
  FInitialDir := Value;
  Changed;
end;

{*****************************************************}

procedure TJvPictureDlgOptions.SetOptions(const Value: TOpenOptions);
begin
  FOptions := Value;
  Changed;
end;

{*****************************************************}

procedure TJvPictureDlgOptions.SetTitle(const Value: string);
begin
  FTitle := Value;
  Changed;
end;

end.
