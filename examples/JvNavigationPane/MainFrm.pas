// $Id$
unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvOutlookBar, ComCtrls, CheckLst, JvComponent,
  JvNavigationPane, ImgList, Menus, JvPageList, JclWin32, JvExControls, ExtCtrls,
  JvExExtCtrls;

type
  TForm1 = class(TForm)
    PopupMenu1: TPopupMenu;
    LargeImages: TImageList;
    HideAll1: TMenuItem;
    ShowAll1: TMenuItem;
    N1: TMenuItem;
    Dontallowresize1: TMenuItem;
    ChangeFont1: TMenuItem;
    SmallImages: TImageList;
    Colors1: TMenuItem;
    Standard1: TMenuItem;
    Blue1: TMenuItem;
    Silver1: TMenuItem;
    Olive1: TMenuItem;
    JvNavPaneStyleManager1: TJvNavPaneStyleManager;
    JvOutlookSplitter1: TJvOutlookSplitter;
    N2: TMenuItem;
    ShowToolPanel1: TMenuItem;
    ToolImages: TImageList;
    ShowCloseButton1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure Dontallowresize1Click(Sender: TObject);
    procedure HideAll1Click(Sender: TObject);
    procedure ShowAll1Click(Sender: TObject);
    procedure ChangeFont1Click(Sender: TObject);
    procedure SchemaClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure ShowToolPanel1Click(Sender: TObject);
    procedure ShowCloseButton1Click(Sender: TObject);
  private
    { Private declarations }
    procedure DoToolMouseDown(Sender:TObject; Button:TMouseButton; Shift:TShiftState; X, Y: integer);
    procedure DoToolMouseMove(Sender:TObject; Shift:TShiftState; X, Y:integer);
    procedure DoToolPanelClose(Sender: TObject);
    procedure DoToolButtonClick(Sender:TObject; Index:integer);
    procedure DoToolEndDock(Sender, Target: TObject; X, Y: Integer);
  public
    { Public declarations }
    NP:TJvNavigationPane;
    NT : TJvNavPaneToolPanel;
  end;

var
  Form1: TForm1;

implementation
uses
  CommCtrl;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  Page:TJvNavPanelPage;
  GH:TJvNavPanelHeader;
  N:TTreeNode;
  R:TRect;
  i:integer;
  function StripAccel(const S:string):string;
  begin
    // quick and dirty ;-)
    Result := StringReplace(S, '&&', #1, [rfReplaceAll]);
    Result := StringReplace(Result, '&', '', [rfReplaceAll]);
    Result := StringReplace(Result, #1, '&', [rfReplaceAll]);
  end;
begin

  JvNavPaneStyleManager1.Theme := nptCustom;
  // this is how to create a NavPane at run-time
  // also shows how to create and insert pages as well as controls on pages
  NP := TJvNavigationPane.Create(Self);

  NP.Parent := Self;
  NP.Cursor := crHandPoint;
  NP.Width := 220;
//  NP.BorderWidth := 2;
  NP.Align := alLeft;
  JvOutlookSplitter1.Left := 225;
  JvOutlookSplitter1.MinSize := 220;
  NP.DropDownMenu := PopupMenu1;
  NP.SmallImages := SmallImages;
  NP.LargeImages := LargeImages;
  NP.StyleManager := JvNavPaneStyleManager1;

  Page := TJvNavPanelPage.Create(Self);
  Page.Caption := '&Mail';
  Page.ImageIndex := 0;
  Page.PageList := NP;
  GH := TJvNavPanelHeader.Create(Self);
  GH.Parent := Page;
  GH.Align := alTop;
  GH.Images := SmallImages;
  GH.ImageIndex := -1;
  GH.Caption := StripAccel(Page.Caption);
  GH.StyleManager := JvNavPaneStyleManager1;
  // use a button here instead of the icon:
  with TJvNavIconButton.Create(Self) do
  begin
    Parent := GH;
    Align := alRight;
    ButtonType := nibImage;
//    Colors.SelectedColorFrom := clNone;
//    Colors.SelectedColorTo := clNone;
    Colors.ButtonHotColorFrom := clNone;
    Colors.ButtonHotColorTo := clNone;
    Images := SmallImages;
    DropDownMenu := PopupMenu1;
    ImageIndex := 0;
    StyleManager := JvNavPaneStyleManager1;
  end;

  with TJvNavPanelDivider.Create(Self) do
  begin
    Caption := 'Favorite Folders';
    Parent := Page;
    Top := 100;
    Align := alTop;
    Enabled := false;
    Cursor := crDefault;
    StyleManager := JvNavPaneStyleManager1;
  end;

  with TTreeView.Create(Self) do
  begin
    Parent := Page;
    Top := 200;
    Align := alTop;
    Font.Style := [];
    BorderStyle := bsNone;
    Items.Add(nil,'Inbox');
    Items.Add(nil,'Unread Mail');
    Items.Add(nil,'For Follow Up [4]');
    Items.Add(nil,'Sent Items');
    Height := 100;
  end;

  with TJvNavPanelDivider.Create(Self) do
  begin
    Caption := 'All Mail Folders';
    Parent := Page;
    Top := 100;
    Align := alTop;
    Cursor := crSizeNS;
    StyleManager := JvNavPaneStyleManager1;
  end;

  with TTreeView.Create(Self) do
  begin
    Parent := Page;
    Align := alClient;
    BorderStyle := bsNone;
    Font.Style := [];
    N := Items.Add(nil,'Mailbox - Chris Gray');
    Items.AddChild(N,'Deleted Items');
    Items.AddChild(N,'Drafts');
    Items.AddChild(N,'Inbox');
    Items.AddChild(N,'Junk E-mail');
    Items.AddChild(N,'Outbox');
    Items.AddChild(N,'Sent Items');
    N := Items.AddChild(N,'Search Folders');
    Items.AddChild(N,'For Follow Up [4]');
    Items.AddChild(N,'Large Mail');
    Items.AddChild(N,'Unread Mail');
    FullExpand;
  end;

  Page := TJvNavPanelPage.Create(Self);
  Page.Caption := '&Calendar';
  Page.ImageIndex := 1;
  Page.PageList := NP;
  GH := TJvNavPanelHeader.Create(Self);
  GH.Parent := Page;
  GH.Align := alTop;
  GH.Caption := StripAccel(Page.Caption);
  GH.Images := SmallImages;
  GH.ImageIndex := Page.ImageIndex;
  GH.StyleManager := JvNavPaneStyleManager1;
  // NB! TMonthCalendar messes up the form when you size the form smaller than one calendar width
  with TMonthCalendar.Create(Self) do
  begin
    Parent := Page;
    Align := alTop;
    AutoSize := true;
    AutoSize := false;
    Date := SysUtils.Date;
    MonthCal_GetMinReqRect(Handle, R);
  end;
  Constraints.MinHeight := R.Bottom - R.Top + 12;
  Constraints.MinWidth := R.Right - R.Left + 12;


  with TJvNavPanelDivider.Create(Self) do
  begin
    Caption := 'My Calendars';
    Parent := Page;
    Top := 1500;
    Align := alTop;
    Cursor := crDefault;
    Enabled := false;
    StyleManager := JvNavPaneStyleManager1;
  end;
  with TCheckListBox.Create(Self) do
  begin
    Parent := Page;
    Checked[Items.Add('Calendar')] := true;
    Items.Add('Project Schedule');
    Top := 1500;
    Height := 32;
    Align := alTop;
  end;
  with TJvNavPanelDivider.Create(Self) do
  begin
    Caption := 'Other Calendars';
    Parent := Page;
    Top := 1500;
    Align := alTop;
    Cursor := crSizeNS;
    StyleManager := JvNavPaneStyleManager1;
  end;
  with TCheckListBox.Create(Self) do
  begin
    Parent := Page;
    Checked[Items.Add('Alan Chong')] := Random(4) = 1;
    Checked[Items.Add('Andreas Hausladen')] := Random(4) = 1;
    Checked[Items.Add('André Snepvangers')] := Random(4) = 1;
    Checked[Items.Add('Michael Beck')] := Random(4) = 1;
    Checked[Items.Add('Leroy Casterline')] := Random(4) = 1;
    Checked[Items.Add('Chris Latta')] := Random(4) = 1;
    Checked[Items.Add('Erwin Molendijk')] := Random(4) = 1;
    Checked[Items.Add('James Lan')] := Random(4) = 1;
    Checked[Items.Add('Ignacio Vazquez')] := Random(4) = 1;
    Checked[Items.Add('Marcel Bestebroer')] := Random(4) = 1;
    Checked[Items.Add('Jens Fudickar')] := Random(4) = 1;
    Checked[Items.Add('Jose Perez')] := Random(4) = 1;
    Checked[Items.Add('Marc Hoffmann')] := Random(4) = 1;
    Checked[Items.Add('Fernando Silva')] := Random(4) = 1;
    Checked[Items.Add('Robert Marquardt')] := Random(4) = 1;
    Checked[Items.Add('Matthias Thoma')] := Random(4) = 1;
    Checked[Items.Add('Olivier Sannier')] := Random(4) = 1;
    Checked[Items.Add('Oliver Giesen')] := Random(4) = 1;
    Checked[Items.Add('Dmitry Osinovsky')] := Random(4) = 1;
    Checked[Items.Add('Peter Thornqvist')] := Random(4) = 1;
    Checked[Items.Add('henri gourvest')] := Random(4) = 1;
    Checked[Items.Add('Rob den Braasem')] := Random(4) = 1;
    Checked[Items.Add('Remko Bonte')] := Random(4) = 1;
    Checked[Items.Add('Christian Vogt')] := Random(4) = 1;
    Checked[Items.Add('Warren Postma')] := Random(4) = 1;
    Top := 1500;
    Align := alClient;
  end;

  Page := TJvNavPanelPage.Create(Self);
  Page.Caption := 'C&ontacts';
  Page.ImageIndex := 2;
  Page.PageList := NP;
  GH := TJvNavPanelHeader.Create(Self);
  GH.Parent := Page;
  GH.Align := alTop;
  GH.Caption := StripAccel(Page.Caption);
  GH.Images := SmallImages;
  GH.ImageIndex := Page.ImageIndex;
  GH.StyleManager := JvNavPaneStyleManager1;
  with TListBox.Create(Self) do
  begin
    Parent := Page;
    Align := alClient;
    Items.Add('Alan Chong');
    Items.Add('Andreas Hausladen');
    Items.Add('André Snepvangers');
    Items.Add('Michael Beck');
    Items.Add('Leroy Casterline');
    Items.Add('Chris Latta');
    Items.Add('Erwin Molendijk');
    Items.Add('James Lan');
    Items.Add('Ignacio Vazquez');
    Items.Add('Marcel Bestebroer');
    Items.Add('Jens Fudickar');
    Items.Add('Jose Perez');
    Items.Add('Marc Hoffmann');
    Items.Add('Fernando Silva');
    Items.Add('Robert Marquardt');
    Items.Add('Matthias Thoma');
    Items.Add('Olivier Sannier');
    Items.Add('Oliver Giesen');
    Items.Add('Dmitry Osinovsky');
    Items.Add('Peter Thornqvist');
    Items.Add('henri gourvest');
    Items.Add('Rob den Braasem');
    Items.Add('Remko Bonte');
    Items.Add('Christian Vogt');
    Items.Add('Warren Postma');
  end;


  Page := TJvNavPanelPage.Create(Self);
  Page.Caption := '&Tasks';
  Page.ImageIndex := 3;
  Page.PageList := NP;
  GH := TJvNavPanelHeader.Create(Self);
  GH.Parent := Page;
  GH.Align := alTop;
  GH.Caption := StripAccel(Page.Caption);
  GH.Images := SmallImages;
  GH.ImageIndex := Page.ImageIndex;
  GH.StyleManager := JvNavPaneStyleManager1;

  Page := TJvNavPanelPage.Create(Self);
  Page.Caption := '&Notes';
  Page.ImageIndex := 4;
  Page.PageList := NP;
  GH := TJvNavPanelHeader.Create(Self);
  GH.Parent := Page;
  GH.Align := alTop;
  GH.Caption := StripAccel(Page.Caption);
  GH.Images := SmallImages;
  GH.ImageIndex := Page.ImageIndex;
  GH.StyleManager := JvNavPaneStyleManager1;


  Page := TJvNavPanelPage.Create(Self);
  Page.Caption := '&Folder List';
  Page.ImageIndex := 5;
  Page.PageList := NP;
  GH := TJvNavPanelHeader.Create(Self);
  GH.Parent := Page;
  GH.Align := alTop;
  GH.Caption := StripAccel(Page.Caption);
  GH.Images := SmallImages;
  GH.ImageIndex := Page.ImageIndex;
  GH.StyleManager := JvNavPaneStyleManager1;

{  with TJvOutlookSplitter.Create(Self) do
  begin
    Align := alNone;
    Parent := Self;
    Left := NP.Width + 100;
    Align := alLeft;
    Width := 7;
    Cursor := crSizeWE;
  end;
  }
  NP.ActivePageIndex := 0;

  NT := TJvNavPaneToolPanel.Create(Self);
  NT.DragKind := dkDock;
//  NT.DragMode := dmAutomatic;
  NT.Parent := Self;
  NT.Align := alClient;
  NT.Caption := 'Sample Tool Panel';
  NT.StyleManager := JvNavPaneStyleManager1;
  NT.Images := ToolImages;
  NT.DropDownMenu := PopupMenu1;
  for i := 0 to ToolImages.Count  - 1 do
    NT.Buttons.Add.ImageIndex := i;
  NT.OnButtonClick := DoToolButtonClick;
  NT.OnMouseDown := DoToolMouseDown;
  NT.OnMouseMove := DoToolMouseMove;
  NT.OnEndDock := DoToolEndDock;

  NT.CloseButton := false;
  NT.OnClose := DoToolPanelClose;
  // now, set the real start theme:
  JvNavPaneStyleManager1.Theme := nptStandard;
end;

procedure TForm1.Dontallowresize1Click(Sender: TObject);
begin
  Dontallowresize1.Checked := not Dontallowresize1.Checked;
  NP.Resizable := not Dontallowresize1.Checked;
end;

procedure TForm1.HideAll1Click(Sender: TObject);
begin
  NP.MaximizedCount := 0;
end;

procedure TForm1.ShowAll1Click(Sender: TObject);
begin
  NP.MaximizedCount := NP.PageCount;
end;

procedure TForm1.ChangeFont1Click(Sender: TObject);
begin
 if NP.NavPanelFont.Style = [fsBold] then
   NP.NavPanelFont.Style := [fsItalic, fsBold]
 else
   NP.NavPanelFont.Style := [fsBold];
end;

procedure TForm1.SchemaClick(Sender: TObject);
begin
  JvNavPaneStyleManager1.Theme := TJvNavPanelTheme((Sender as TMenuItem).Tag);
  (Sender as TMenuItem).Checked := true;
end;

procedure TForm1.DoToolPanelClose(Sender: TObject);
begin
  if MessageDlg('Close this window?',mtConfirmation, [mbYes, mbNo],0) = mrYes	then
  begin
    JvOutlookSplitter1.Visible := False;
    NT.Visible := False;
    NP.Align := alClient;
  end;
end;

procedure TForm1.PopupMenu1Popup(Sender: TObject);
begin
  ShowToolPanel1.Enabled := not NT.Visible;
  ShowCloseButton1.Checked := NT.CloseButton;
end;

procedure TForm1.ShowToolPanel1Click(Sender: TObject);
begin
  NP.Align := alLeft;
  NP.Width := 220;
  JvOutlookSplitter1.Visible := True;
  JvOutlookSplitter1.Left := 225;
  NT.Visible := True;
end;

procedure TForm1.DoToolButtonClick(Sender: TObject; Index: integer);
begin
  ShowMessageFmt('You clicked button %d ',[Index]);
end;

procedure TForm1.ShowCloseButton1Click(Sender: TObject);
begin
  ShowCloseButton1.Checked := not ShowCloseButton1.Checked;
  NT.CloseButton := ShowCloseButton1.Checked;
end;

type
  THackForm = class(TCustomForm);

procedure TForm1.DoToolEndDock(Sender, Target: TObject; X, Y: Integer);
begin
  if (Target is TCustomForm) and (Target <> Self) then
  begin
    TCustomForm(Target).BorderStyle := bsSizeable;
    SetWindowLong(TCustomForm(Target).Handle, GWL_STYLE, GetWindowLong(TCustomForm(Target).Handle, GWL_STYLE) and not WS_CAPTION);
    TCustomForm(Target).Width := TCustomForm(Target).Width + 1;
    TCustomForm(Target).Width := TCustomForm(Target).Width - 1;
  end
  else
    NT.Align := alClient;
end;

procedure TForm1.DoToolMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if phtGrabber in NT.GetHitTestInfoAt(X,Y) then
    NT.BeginDrag(false);
end;

procedure TForm1.DoToolMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: integer);
begin
  if phtGrabber in NT.GetHitTestInfoAt(X,Y) then
    NT.Cursor := crSize
  else
    NT.Cursor := crDefault;
end;

end.


