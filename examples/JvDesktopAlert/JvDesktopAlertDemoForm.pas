{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:
 
 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.delphi-jedi.org

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}
unit JvDesktopAlertDemoForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtDlgs, ImgList, Menus, StdCtrls, ComCtrls,
  ExtCtrls, JvDesktopAlert, JvAppStorage, JvAppIniStorage, JvComponent,
  JvFormPlacement, JvBaseDlg, JvExControls, JvLabel, JvComponentBase;

type
  TJvDesktopAlertDemoFrm = class(TForm)
    btnPreview: TButton;
    PopupMenu1: TPopupMenu;
    Examplemenu1: TMenuItem;
    N1: TMenuItem;
    Clickme1: TMenuItem;
    ImageList1: TImageList;
    Label1: TLabel;
    edHeader: TEdit;
    Label2: TLabel;
    edMessage: TEdit;
    Label3: TLabel;
    btnBrowse: TButton;
    pnlImage: TPanel;
    Image1: TImage;
    Label4: TLabel;
    edButtons: TEdit;
    udButtons: TUpDown;
    chkClickable: TCheckBox;
    chkMovable: TCheckBox;
    chkClose: TCheckBox;
    Label5: TLabel;
    edWindowCount: TEdit;
    udWindowCount: TUpDown;
    OpenPictureDialog1: TOpenPictureDialog;
    chkShowDropDown: TCheckBox;
    Label6: TLabel;
    edStartInterval: TEdit;
    udFadeIn: TUpDown;
    Label7: TLabel;
    edDisplayTime: TEdit;
    udWait: TUpDown;
    Label8: TLabel;
    edEndInterval: TEdit;
    udFadeOut: TUpDown;
    cbLocation: TComboBox;
    Label9: TLabel;
    JvFormStorage1: TJvFormStorage;
    JvAppIniFileStorage1: TJvAppIniFileStorage;
    gbFormSize: TGroupBox;
    edWidth: TEdit;
    edHeight: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    edEndSteps: TEdit;
    udEndSteps: TUpDown;
    Label13: TLabel;
    edStartSteps: TEdit;
    udStartSteps: TUpDown;
    cmbStyle: TComboBox;
    Label14: TLabel;
    chkUseDLL: TCheckBox;
    procedure btnPreviewClick(Sender: TObject);
    procedure Clickme1Click(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FCount:integer;
    FLibHandle:Cardinal;
    procedure DoButtonClick(Sender: TObject);
    procedure DoMessageClick(Sender: TObject);
    procedure DoAlertClose(Sender: TObject);
    procedure DoAlertShow(Sender: TObject);
    procedure LoadDLL;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TCallDesktopAlert = procedure (const Title, Message:WideString);stdcall;

var
  JvDesktopAlertDemoFrm: TJvDesktopAlertDemoFrm;
  CallDesktopAlert:TCallDesktopAlert = nil;

implementation

{$R *.dfm}

procedure TJvDesktopAlertDemoFrm.DoMessageClick(Sender:TObject);
begin
  ShowMessage('You clicked the message!');
end;

procedure TJvDesktopAlertDemoFrm.DoButtonClick(Sender:TObject);
begin
  with Sender as TControl do
    ShowMessageFmt('You clicked button %d!',[Tag]);
end;

procedure TJvDesktopAlertDemoFrm.DoAlertShow(Sender: TObject);
begin
  Inc(FCount);
  Caption := Format('JvDesktopAlert Demo: showing %d alerts',[FCount]);
end;

procedure TJvDesktopAlertDemoFrm.DoAlertClose(Sender: TObject);
begin
  Dec(FCount);
//  if FCount < 0 then FCount := 0;
  Caption := Format('JvDesktopAlert Demo: showing %d alerts',[FCount]);
end;

procedure TJvDesktopAlertDemoFrm.btnPreviewClick(Sender: TObject);
var
  i,j:integer;
  DA:TJvDesktopAlert;
  FOptions:TJvDesktopAlertOptions;
begin
  if chkUseDLL.Enabled and chkUseDLL.Checked then
  begin
    CallDesktopAlert(edHeader.Text, edMessage.Text);
    Exit;
  end;

  for i := 0 to udWindowCount.Position - 1 do
  begin
    DA := TJvDesktopAlert.Create(Self);
    DA.AutoFree := true;
    DA.Images := ImageList1;
    DA.HeaderText := Format('%s (%d)', [edHeader.Text, FCount]);
    DA.MessageText := edMessage.Text;
    DA.Image := Image1.Picture;
    DA.OnMessageClick := DoMessageClick;
    DA.OnShow := DoAlertShow;
    DA.OnClose := DoAlertClose;
    FOptions := [];
    DA.Options := FOptions;
    DA.Location.AlwaysResetPosition := false;
    DA.Location.Position := TJvDesktopAlertPosition(cbLocation.ItemIndex);
    DA.Location.Width := StrToIntDef(edWidth.Text,0);
    DA.Location.Height := StrToIntDef(edHeight.Text,0);
    if DA.Location.Position = dapCustom then
    begin
      DA.Location.Left := Random(Screen.Width - 200);
      DA.Location.Top :=  Random(Screen.Height - 100);
    end;
    DA.AlertStyle := TJvAlertStyle(cmbStyle.ItemIndex);
    DA.StyleHandler.StartInterval := udFadeIn.Position;
    DA.StyleHandler.StartSteps := udStartSteps.Position;
    DA.StyleHandler.DisplayDuration  := udWait.Position; //  + Random(WaitTime);
    DA.StyleHandler.EndInterval := udFadeOut.Position;
    DA.StyleHandler.EndSteps := udEndSteps.Position;
    if chkClickable.Checked then
      Include(FOptions,daoCanClick);
    if chkMovable.Checked then
      Include(FOptions, daoCanMove);
    if chkClose.Checked then
      Include(FOptions, daoCanClose);
    DA.Options := FOptions;
    if chkShowDropDown.Checked then
      DA.DropDownMenu := PopupMenu1;
    for j := 0 to udButtons.Position-1 do
    begin
      with DA.Buttons.Add do
      begin
        ImageIndex := Random(ImageList1.Count);
        Tag := j;
        OnClick := DoButtonClick;
      end;
    end;
    DA.Execute;
  end;
end;

procedure TJvDesktopAlertDemoFrm.Clickme1Click(Sender: TObject);
begin
  ShowMessage('You clicked the menu!');
end;

procedure TJvDesktopAlertDemoFrm.btnBrowseClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
    Image1.Picture.LoadFromFile(OpenPictureDialog1.Filename);
end;

procedure TJvDesktopAlertDemoFrm.FormCreate(Sender: TObject);
begin
  cbLocation.ItemIndex := 3;
  LoadDLL;
end;

constructor TJvDesktopAlertDemoFrm.Create(AOwner: TComponent);
begin
  inherited;
  cmbStyle.ItemIndex := 0;
end;

procedure TJvDesktopAlertDemoFrm.LoadDLL;
begin
  FLibHandle := SafeLoadLibrary('JvDesktopAlertDLL');
  if FLibHandle <> 0 then
    @CallDesktopAlert := GetProcAddress(FLibHandle, 'CallDesktopAlert');
  chkUseDLL.Enabled := Assigned(CallDesktopAlert);
end;

destructor TJvDesktopAlertDemoFrm.Destroy;
begin
  if FLibHandle <> 0 then
    FreeLibrary(FLibHandle);
  FLibHandle := 0;
  inherited;
end;

end.