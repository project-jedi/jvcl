unit Main;

interface

uses
  Windows, Messages, SysUtils, {$IFDEF DELPHI6_UP}Variants, {$ENDIF} Classes, Graphics, Controls, Forms,
  Dialogs, JvOLBar, ComCtrls, StdCtrls, Menus, ExtCtrls, ImgList;

type
  TForm1 = class(TForm)
    popOL: TPopupMenu;
    Splitter1: TSplitter;
    ImageList1: TImageList;
    ImageList2: TImageList;
    Image1: TImage;
    Image2: TImage;
    Defaultpopupmenu1: TMenuItem;
    popButton: TPopupMenu;
    popPage: TPopupMenu;
    ImageList3: TImageList;
    Editbuttoncaption1: TMenuItem;
    Editpagecaption1: TMenuItem;
    StatusBar1: TStatusBar;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Editpagecaption1Click(Sender: TObject);
    procedure Editbuttoncaption1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    OL:TJvOutlookBar;
    procedure DoButtonClick(Sender:TObject; Index:integer);
    procedure DoPageChange(Sender: TObject; Index: integer);
    procedure DoPageChanging(Sender: TObject; Index: integer;
      var AllowChange: boolean);
    procedure OLContextPopUp(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.DoPageChange(Sender:TObject;Index:integer);
begin
  if Index > -1 then
    Caption := Format('Page changed to "%s"',[OL.Pages[Index].Caption]);
end;

procedure TForm1.DoPageChanging(Sender:TObject;Index:integer;var AllowChange:boolean);
begin
  if (OL.ActivePageIndex > -1) and (Index > -1) then
  begin
    Caption := Format('Page changing from "%s" to "%s"',[OL.Pages[OL.ActivePageIndex].Caption,OL.Pages[Index].Caption]);
//    sleep(200);
  end;
end;

procedure TForm1.DoButtonClick(Sender:TObject; Index:integer);
var P:TJvOutlookBarPage;
begin
  if (Index > -1) then
  begin
    P := OL.Pages[OL.ActivePageIndex];
    Caption := Format('Clicked button "%s" on page "%s"',[P.Buttons[Index].Caption,P.Caption]);
  end;
end;

procedure TForm1.OLContextPopUp(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
  if OL.PopupObject is TJvOutlookBarPage then
    OL.PopUpMenu := popPage
  else if OL.PopupObject is TJvOutlookBarButton then
    OL.PopUpMenu := popButton
  else
    OL.PopUpMenu := popOL;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  OL := TJvOutlookBar.Create(self);
  OL.Constraints.MaxWidth := 300;
  OL.Constraints.MinWidth := 150;
  OL.OnPageChange := DoPageChange;
  OL.OnPageChanging := DoPageChanging;
  OL.OnButtonClick  := DoButtonClick;
  OL.OnContextPopUp := OLContextPopUp;
  OL.BorderStyle := bsSingle;
  OL.Align := alLeft;
//  OL.Anchors := [akTop,akLeft,akRight];

  OL.Parent := self;
//  OL.BorderStyle := bsNone; // placing this to early (i.e right after .Create above) has no effect ???
  OL.LargeImages := ImageList1;
  OL.SmallImages := ImageList2;
  OL.Font.Name := 'Verdana';
  OL.Font.Size := 10;
  OL.ButtonSize := olbsLarge;
  OL.Color := clGray;
  OL.PopUpMenu := popOL;
  with OL.Pages.Add do
  begin
    Image := Image2.Picture.Bitmap;
    Caption := 'Page 0';
    with Buttons.Add do
    begin
      Caption := 'Button 1';
      ImageIndex := 0;
    end;
    with Buttons.Add do
    begin
      Caption := 'Button 2';
      ImageIndex := 1;
    end;
    with Buttons.Add do
    begin
      Caption := 'Button 3';
      ImageIndex := 2;
    end;
    with Buttons.Add do
    begin
      Caption := 'Button 4';
      ImageIndex := 3;
    end;
    Color := clRed;
  end;
  with OL.Pages.Add do
  begin
    Caption := 'Page 1';
    // no buttons
  end;

  with OL.Pages.Add do
  begin
    Image := Image1.Picture.Bitmap;
    Caption := 'Page 2';
    with Buttons.Add do
      Caption := 'Button 4';
    with Buttons.Add do
      Caption := 'Button 3';
    with Buttons.Add do
      Caption := 'Button 2';
    with Buttons.Add do
      Caption := 'Button 1';
//    Color := clBlue;
  end;
  with OL.Pages.Add do
  begin
    ButtonSize := olbsSmall;
    Caption := 'Page 3';
//    Color := clBtnFace;
    with Buttons.Add do
      Caption := 'Button 1';
    with Buttons.Add do
      Caption := 'Button 2';
    with Buttons.Add do
      Caption := 'Button 3';
    with Buttons.Add do
      Caption := 'Button 4';
    with Buttons.Add do
      Caption := 'Button 5';
    with Buttons.Add do
      Caption := 'Button 6';
    with Buttons.Add do
      Caption := 'Button 7';
    with Buttons.Add do
      Caption := 'Button 8';
    with Buttons.Add do
      Caption := 'Button 9';
    with Buttons.Add do
      Caption := 'Button 10';
    with Buttons.Add do
      Caption := 'Button 11';
    with Buttons.Add do
      Caption := 'Button 12';
    with Buttons.Add do
      Caption := 'Button 13';
    with Buttons.Add do
      Caption := 'Button 14';
    with Buttons.Add do
      Caption := 'Button 15';
    with Buttons.Add do
      Caption := 'Button 16';
    with Buttons.Add do
      Caption := 'Button 17';
    with Buttons.Add do
      Caption := 'Button 18';
    with Buttons.Add do
      Caption := 'Button 19';
    with Buttons.Add do
      Caption := 'Button 20';
  end;
  with OL.Pages.Add do
  begin
    Caption := 'Page 4';
    Color := clBtnFace;
    Font.Color := clBlack;
    with Buttons.Add do
      Caption := 'Button 1';
    with Buttons.Add do
      Caption := 'Button 2';
  end;
  Splitter1.Align := alLeft;
  OL.ActivePageIndex := 2;
  OL.Pages[2].TopButtonIndex := 3;
end;

procedure TForm1.Editpagecaption1Click(Sender: TObject);
begin
  with OL.PopUpObject as TJvOutlookBarPage do
    EditCaption;
end;

procedure TForm1.Editbuttoncaption1Click(Sender: TObject);
begin
  with OL.PopUpObject as TJvOutlookBarButton do
    EditCaption;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  OL.LargeImages := ImageList1;
  OL.SmallImages := ImageList2;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  OL.LargeImages := nil;
  OL.SmallImages := nil;
end;

procedure TForm1.Button3Click(Sender: TObject);
var i:integer;
begin
  with TFontDialog.Create(nil) do
  try
    Font := OL.Font;
    if Execute then
    begin
      for i := 0 to OL.Pages.Count - 1 do
        OL.Pages[i].Font := Font;
      OL.Font := Font;
    end;
  finally
    Free;
  end;
end;

end.
