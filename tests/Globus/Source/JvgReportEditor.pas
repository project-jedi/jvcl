{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgReportEditor.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvgReportEditor;

interface

uses
  Windows,
  JvgReport,
  JvgCaption,
  JvgBevel,
  JvgPage,
  Printers,
  JvgLabel,
  JvgRuler,
  Mask,
  JvgListBox,
  {$IFDEF COMPILER6_UP}
  DesignIntf,
  DesignEditors,
  PropertyCategories,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvComponent,

  Spin,
  JvgReportParamEditorForm,
  Menus,
  ExtCtrls,
  StdCtrls,
  Buttons,
  ComCtrls,
  Controls,
  Dialogs,
  Forms,
  Classes,
  Sysutils,
  graphics
  {$IFDEF COMPILER5_UP},
  Imglist{$ENDIF};

type

  TJvgRepProperty = class(TPropertyEditor)
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

  TJvgReportCompEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TJvgReportEditor = class(TJvComponent)
    FReport: TJvgReport;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    procedure Preview;
    procedure Edit;
  published
    property Report: TJvgReport read FReport write FReport;
  end;

  TJvgReportEditorForm = class(TForm)
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    PM_Control: TPopupMenu;
    N_Linktofile: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    PC: TJvgPageControl;
    Panel1: TPanel;
    Bevel4: TBevel;
    P_Sides: TPanel;
    Panel2: TPanel;
    glBevel1: TJvgBevel;
    Bevel2: TBevel;
    Bevel1: TBevel;
    B_Label: TSpeedButton;
    sb_Open: TSpeedButton;
    sb_Save: TSpeedButton;
    sb_Preview: TSpeedButton;
    sb_Book: TSpeedButton;
    sb_Album: TSpeedButton;
    Bevel3: TBevel;
    sb_OLE: TSpeedButton;
    sb_SnapToGrid: TSpeedButton;
    b_Bevel: TSpeedButton;
    sb_Print: TSpeedButton;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Memo1: TMemo;
    P_Font: TPanel;
    ColorDialog1: TColorDialog;
    N3: TMenuItem;
    N_DeleteObject: TMenuItem;
    P_HRuler: TPanel;
    P_Main: TPanel;
    ScrollBox_: TScrollBox;
    ShapeSize: TShape;
    P_VRuler: TPanel;
    TabSheet3: TTabSheet;
    ImageList1: TImageList;
    HRuler: TJvgRuler;
    VRuler: TJvgRuler;
    glBevel4: TJvgBevel;
    sb_FixAllMoving: TSpeedButton;
    sb_FixMoving: TSpeedButton;
    glLabel3: TLabel;
    N_OLESize: TMenuItem;
    N_Clip: TMenuItem;
    N_Center: TMenuItem;
    N_Scale: TMenuItem;
    N_Stretch: TMenuItem;
    N_AutoSize: TMenuItem;
    P_SBar: TPanel;
    Panel5: TPanel;
    glLabel4: TLabel;
    glLabel5: TLabel;
    glLabel7: TLabel;
    se_Width: TSpinEdit;
    se_Top: TSpinEdit;
    se_Left: TSpinEdit;
    glLabel6: TLabel;
    se_Height: TSpinEdit;
    cb_Components: TComboBox;
    glLabel8: TLabel;
    N4: TMenuItem;
    Bevel5: TBevel;
    SB_Left: TSpeedButton;
    SB_Bottom: TSpeedButton;
    SB_Right: TSpeedButton;
    SB_Top: TSpeedButton;
    sb_AlignL: TSpeedButton;
    sb_AlignC: TSpeedButton;
    sb_AlignR: TSpeedButton;
    sb_AlignW: TSpeedButton;
    RxSpeedButton8: TSpeedButton;
    RxSpeedButton9: TSpeedButton;
    sb_BevelBold: TSpeedButton;
    glBevel2: TJvgBevel;
    Panel3: TPanel;
    RxSpinEdit1: TSpinEdit;
    Panel6: TPanel;
    sbFontColor: TSpeedButton;
    glBevel3: TJvgBevel;
    Panel7: TPanel;
    Edit1: TMemo;
    FE_OLE: TEdit;
    SpeedButton1: TSpeedButton;
    OpenOLEFile: TOpenDialog;
    sb_FontUnderline: TSpeedButton;
    sb_FontItalic: TSpeedButton;
    sb_FontBold: TSpeedButton;
    TabSheet4: TTabSheet;
    glLabel1: TLabel;
    lb_Params: TJvgListBox;
    Panel4: TPanel;
    SpeedButton2: TSpeedButton;
    sbBackColor: TSpeedButton;
    SpeedButton3: TSpeedButton;
    CheckBox1: TCheckBox;
    FontComboBox1: TComboBox;
    ColorComboBox1: TComboBox;
    procedure OpenClick(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure ScrollBox_MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure SB_LeftClick(Sender: TObject);
    procedure FontComboBox1Change(Sender: TObject);
    procedure RxSpinEdit1Change(Sender: TObject);
    procedure ColorComboBox1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sb_BookClick(Sender: TObject);
    procedure N1Click(Sender: TObject);
    procedure sb_FontBoldClick(Sender: TObject);
    procedure sb_AlignLClick(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure sbFontColorClick(Sender: TObject);
    procedure N_DeleteObjectClick(Sender: TObject);
    procedure ScrollBox_MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ScrollBox_MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure sb_FixMovingClick(Sender: TObject);
    procedure N_AutoSizeClick(Sender: TObject);
    procedure sb_SnapToGridClick(Sender: TObject);
    procedure se_SizeChange(Sender: TObject);
    procedure cb_ComponentsChange(Sender: TObject);
    procedure N4Click(Sender: TObject);
    procedure sb_BevelBoldClick(Sender: TObject);
    procedure se_TopChange(Sender: TObject);
    procedure se_WidthChange(Sender: TObject);
    procedure se_HeightChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FE_OLEChange(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure sb_PrintClick(Sender: TObject);
    procedure sb_PreviewClick(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure cb_ComponentsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CheckBox1Click(Sender: TObject);
  private
    ReportParamEditor: TJvgReportParamEditor;

    fMouseDown, fCanUndo, fSkipSizeUpdate: boolean;
    UndoPosShift, SelectedControlLastPos: TPoint;
    ControlPos, SelPt, Grid, Step: TPoint;
    ScrollBox: TJvgRepScrollBox;
    SelectedControl: TJvgReportItem;
    fSelection: boolean;
    SelectionRect: TRect;
    procedure RemakeComponentsList;
    procedure read(FileName: string; ParentWnd: TWinControl);
    procedure Save(FileName: string);
    procedure OnMouseDown_(Sender: TObject; Button: TMouseButton; Shift:
      TShiftState; X, Y: Integer);
    procedure OnMouseUp_(Sender: TObject; Button: TMouseButton; Shift:
      TShiftState; X, Y: Integer);
    procedure OnMouseMove_(Sender: TObject; Shift: TShiftState; X, Y:
      Integer);
    procedure OnResize_(Sender: TObject);
    procedure OnDrawScrollBox(Sender: TObject);
    procedure UpdatePageSize;
    procedure ResizeReportControls(l, t, w, h: integer; fUseParamsAsShifts:
      boolean);
    procedure ShowComponentPos(Control: TControl);
    procedure AssignEventsToAllComponents;
    procedure UpdateToolBar(Control: TJvgReportItem);
  public
    Component: TJvgReport;
    procedure Preview(JvgReport: TJvgReport);
    procedure Edit(JvgReport: TJvgReport);
  end;

  TJvgPublicControl = class(TControl)
  public
    property Caption;
  end;

  TJvgPublicControlClass = class of TJvgPublicControl;

const
  IGNORE_VALUE = 65536;
var
  glRepEditor: TJvgReportEditorForm;
  Form2: TComponent;

implementation
uses JvgTypes,
  JvgUtils, {PrintR,} { ConfirmF, AboutF,}
  JvgAlignFunction,
  JvgAlignForm;
{$R *.DFM}
//----------- common proc

procedure ShowReportEditor(JvgReport: TComponent; fEdit: boolean);
var
  Dialog: TJvgReportEditorForm;
  Report: TJvgReport;
begin
  Dialog := TJvgReportEditorForm.Create(Application);
  if JvgReport is TJvgReport then
    Report := TJvgReport(JvgReport)
  else
    Report := TJvgReportEditor(JvgReport).Report;
  if Report = nil then
    exit;
  if fEdit then
    Dialog.Edit(Report)
  else
    Dialog.Preview(Report);
  Dialog.free;
end;

//----------- TJvgComponentListProperty

function TJvgRepProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TJvgRepProperty.GetValue: string;
begin
  Result := Format('(%s)', [GetPropType^.Name]);
end;

procedure TJvgRepProperty.Edit;
begin
  ShowReportEditor(TJvgReport(GetComponent(0)), true);
end;
//----------- TJvgReportCompEditor

procedure TJvgReportCompEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: ShowReportEditor(Component, true);
    1: ShowReportEditor(Component, false);
  end;
end;

function TJvgReportCompEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Edit report...';
    1: Result := 'Preview report...';
  end;
end;

function TJvgReportCompEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;
//----------- TJvgReportEditor

procedure TJvgReportEditor.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  if (AComponent = Report) and (Operation = opRemove) then
    Report := nil;
  inherited;
end;

procedure TJvgReportEditor.Preview;
begin
  ShowReportEditor(self, false);
end;

procedure TJvgReportEditor.Edit;
begin
  ShowReportEditor(self, true);
end;
//----------- TJvgReportEditorForm

procedure TJvgReportEditorForm.Preview(JvgReport: TJvgReport);
begin
  PC.Visible := false;
  ScrollBox.Enabled := false;
  Component := JvgReport;
  ShowModal;
end;

procedure TJvgReportEditorForm.Edit(JvgReport: TJvgReport);
begin
  PC.Visible := true;
  ScrollBox.Enabled := true;
  Component := JvgReport;
  ShowModal;
end;

procedure TJvgReportEditorForm.OnMouseDown_(Sender: TObject; Button:
  TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  R: TRect;
  pt: TPoint;
  i: integer;
begin
  if TControl(Sender).Cursor <> crDefault then
    exit;
  ActiveControl := nil;
  fCanUndo := false;
  if (B_Label.Down) or (B_Bevel.Down) or (sb_OLE.Down) or (ssCtrl in Shift) then
  begin
    ScrollBox_MouseDown(Sender, Button, Shift, X + TControl(Sender).Left, Y +
      TControl(Sender).Top);
    exit;
  end;
  ScrollBox_MouseDown(Sender, Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    fMouseDown := true;
    TControl(Sender).Tag := 1;
    ControlPos.x := X;
    ControlPos.y := Y;
  end
  else if Button = mbRight then
  begin
    OnMouseDown_(Sender, mbLeft, Shift, X, Y);
    pt.x := X;
    pt.y := Y;
    pt := SelectedControl.ClientToScreen(pt);
    N_Linktofile.Enabled := SelectedControl.ContainOLE;
    N_OLESize.Enabled := SelectedControl.ContainOLE;
    case SelectedControl.OLESizeMode of
      0: N_Clip.Checked := true;
      1: N_Center.Checked := true;
      2: N_Scale.Checked := true;
      3: N_Stretch.Checked := true;
      4: N_AutoSize.Checked := true;
    end;

    PM_Control.Popup(pt.X, pt.Y);
    OnMouseUp_(Sender, Button, Shift, X, Y);
  end;

  if Assigned(SelectedControl) then
    if ssShift in Shift then
    begin
      TJvgReportItem(Sender).Selected := not TJvgReportItem(Sender).Selected;
      fMouseDown := false;
      if SelectedControl = Sender then
        exit;
    end
    else
    begin
      if TJvgReportItem(Sender).Selected then
        exit;
      with ScrollBox do
        for i := 0 to ControlCount - 1 do
          if (Controls[i] is TJvgReportItem) and
            TJvgReportItem(Controls[i]).Selected then
            TJvgReportItem(Controls[i]).Selected := false;
      SelectedControl.Selected := false;
      SelectedControl.Invalidate;
    end;

  SelectedControl := TJvgReportItem(Sender);
  SelectedControl.Selected := true;

  UpdateToolBar(SelectedControl);
  SelectedControlLastPos.X := SelectedControl.Left;
  SelectedControlLastPos.Y := SelectedControl.Top;

end;

procedure TJvgReportEditorForm.OnMouseUp_(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  R: TRect;
begin
  ScrollBox_MouseUp(Sender, Button, Shift, X, Y);
  fMouseDown := false;
  if not Assigned(SelectedControl) then
    exit;
  UndoPosShift.X := SelectedControlLastPos.X - SelectedControl.Left;
  UndoPosShift.Y := SelectedControlLastPos.Y - SelectedControl.Top;
  fCanUndo := true;
  //if not Assigned(SelectedControl) then exit;
  //if TControl(Sender).Tag = 0 then exit;
{  DrawFocusRect( TControl(Sender).Parent.Handle, FocusRect );
  R := Rect( 0, 0, Screen.Width, Screen.Height );
  ClipCursor( @R );
  TControl(Sender).Left := TControl(Sender).Left + X - ControlPos.x;
  TControl(Sender).Top := TControl(Sender).Top + Y - ControlPos.y;
  if TControl(Sender).Tag = 2 then//...was moved
    TJvgReportItem(Sender).RepaintBorder;}
  TControl(Sender).Tag := 0;
end;

procedure TJvgReportEditorForm.OnMouseMove_(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  DC: HDC;
  i: integer;
begin
  if fSelection then
    ScrollBox_MouseMove(Sender, Shift, X, Y);
  if sb_FixAllMoving.Down then
    exit;
  //  if TControl(Sender).Tag = 0 then exit;
  if not fMouseDown then
    exit;
  with ScrollBox do
    for i := 0 to ControlCount - 1 do
      if (Controls[i] is TJvgReportItem) then
        with TJvgReportItem(Controls[i]) do
          if Selected and not bool(Fixed) then
          begin
            Left := ((Left + X - ControlPos.x) div Step.X) * Step.X;
            Top := ((Top + Y - ControlPos.y) div Step.Y) * Step.Y;
          end;
  fSkipSizeUpdate := true;
  ShowComponentPos(SelectedControl);
  //TControl(Sender).Left := TControl(Sender).Left + X - ControlPos.x;
  //TControl(Sender).Top := TControl(Sender).Top + Y - ControlPos.y;
  {
  TControl(Sender).Tag := 2;//...on moving
  DC := GetDC( TControl(Sender).Parent.Handle );
  DrawFocusRect( DC, FocusRect );
  FocusRect := Bounds( TControl(Sender).Left+X-ControlPos.x, TControl(Sender).Top+Y-ControlPos.y,  SelectedControl.Width, SelectedControl.Height );
  DrawFocusRect( DC, FocusRect );
  ReleaseDC( TControl(Sender).Parent.Handle, DC );
  }
end;

procedure TJvgReportEditorForm.OnResize_(Sender: TObject);
begin
  fSkipSizeUpdate := true;
  if Sender = SelectedControl then
    ShowComponentPos(TControl(Sender));
end;

procedure TJvgReportEditorForm.read(FileName: string; ParentWnd: TWinControl);
begin
  ScrollBox.HorzScrollBar.Position := 0;
  ScrollBox.VertScrollBar.Position := 0;
  SelectedControl := nil;
  UpdateToolBar(nil);
  Component.LoadFromFile(FileName);
  Component.CreateReport(ParentWnd, true);
  AssignEventsToAllComponents;
  RemakeComponentsList;
end;

procedure TJvgReportEditorForm.Save(FileName: string);
begin
  ScrollBox.HorzScrollBar.Position := 0;
  ScrollBox.VertScrollBar.Position := 0;
  Component.SaveToFile(FileName);
end;

procedure TJvgReportEditorForm.OpenClick(Sender: TObject);
begin
  OpenDialog1.InitialDir := ExtractFilePath(ParamStr(0));
  if OpenDialog1.Execute then
    Read(OpenDialog1.FileName, ScrollBox);
end;

procedure TJvgReportEditorForm.Save1Click(Sender: TObject);
begin
  SaveDialog1.InitialDir := ExtractFilePath(ParamStr(0));
  if SaveDialog1.Execute then
    Save(SaveDialog1.FileName);
end;

procedure TJvgReportEditorForm.ScrollBox_MouseDown(Sender: TObject; Button:
  TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  l, Compon: TJvgReportItem;
  R: TRect;
  pt: TPoint;
begin
  if ssCtrl in Shift then
  begin
    SelectionRect := Rect(0, 0, 0, 0);
    SelPt.X := X - ScrollBox.HorzScrollBar.Position;
    SelPt.Y := Y - ScrollBox.VertScrollBar.Position;
    SelPt := ScrollBox.ClientToScreen(SelPt);
    fSelection := true;
  end;
  if (B_Label.Down) or (B_Bevel.Down) or (sb_OLE.Down) then
  begin
    Compon := Component.AddComponent;
    with Compon do
    begin
      Left := X - ScrollBox.HorzScrollBar.Position;
      Top := Y - ScrollBox.VertScrollBar.Position;
      if B_Label.Down then
      begin
        Text := 'Label';
        SideLeft := 0;
        SideTop := 0;
        SideRight := 0;
        SideBottom := 0;
      end;
      OnMouseDown := OnMouseDown_;
      OnMouseUp := OnMouseUp_;
      OnMouseMove := OnMouseMove_;
      OnResize := OnResize_;
      ContainOLE := sb_OLE.Down;
      B_Label.Down := false;
      B_Bevel.Down := false;
      sb_OLE.Down := false;
      RemakeComponentsList;
    end;
  end
  else
  begin
    R := ScrollBox.ClientRect;
    pt.x := 0;
    pt.y := 0;
    pt := ScrollBox.ClientToScreen(pt);
    OffsetRect(R, pt.x, pt.y);
    ClipCursor(@R);
  end;
end;

procedure TJvgReportEditorForm.ScrollBox_MouseMove(Sender: TObject; Shift:
  TShiftState;
  X, Y: Integer);
var
  DC: HDC;
  pt: TPoint;
begin

  if not fSelection then
    exit;
  DC := GetDC(0);
  DrawFocusRect(DC, SelectionRect);
  Pt.X := X - ScrollBox.HorzScrollBar.Position;
  Pt.Y := Y - ScrollBox.VertScrollBar.Position;
  pt := ScrollBox.ClientToScreen(pt);
  SelectionRect := Bounds(min(SelPt.X, pt.X), min(SelPt.Y, pt.Y), abs(SelPt.X -
    pt.X), abs(SelPt.Y - pt.Y));
  DrawFocusRect(DC, SelectionRect);
  ReleaseDC(0, DC);
end;

procedure TJvgReportEditorForm.ScrollBox_MouseUp(Sender: TObject; Button:
  TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  DC: HDC;
  R: TRect;
  i: integer;
begin
  if fSelection then
  begin
    DC := GetDC(0);
    DrawFocusRect(DC, SelectionRect);
    ReleaseDC(0, DC);
    fSelection := false;
    //...select all in rect
    with Component.ParentWnd do
      for i := 0 to ControlCount - 1 do
        if Controls[i] is TJvgReportItem then
          with TJvgReportItem(Controls[i]) do
          begin
            R := ClientRect;
            OffsetRect(R, ClientOrigin.x, ClientOrigin.y);
            if IntersectRect(R, R, SelectionRect) then
              Selected := true
            else
              Selected := false;
          end;

  end;
  R := Rect(0, 0, Screen.Width, Screen.Height);
  ClipCursor(@R);
end;

procedure TJvgReportEditorForm.FormCreate(Sender: TObject);
var
  R: TRect;
begin
  R := Rect(0, 0, Screen.Width, Screen.Height);
  ClipCursor(@R);
  ScrollBox := TJvgRepScrollBox.Create(self);
  ScrollBox.Align := alClient;
  //  ScrollBox.Color := clWhite;
  ShapeSize.Parent := ScrollBox;
  ScrollBox.OnDraw := OnDrawScrollBox;
  ScrollBox.Parent := P_Main;
  ScrollBox.Tag := 1; //...1 - draw report controls with dot bevel
  ScrollBox.OnMouseDown := ScrollBox_MouseDown;
  //  ScrollBox.VertScrollBar.Tracking := true;
  //  ScrollBox.HorzScrollBar.Tracking := true;
  ShapeSize.Parent := ScrollBox;
  ShapeSize.Left := 0;
  ShapeSize.Top := 0;
  //  LaccReport := TJvgReport.Create;
  Grid.X := 8;
  Grid.Y := 8;
  Step.X := 1;
  Step.Y := 1;
  {  HRuler := TJvgRuler.Create(self);
    HRuler.Orientation := goHorizontal;
    HRuler.Parent := P_HRuler;
    HRuler.Top := 0;
    HRuler.Height := P_HRuler.Height;

    VRuler := TJvgRuler.Create(self);
    VRuler.Orientation := goVertical;
    VRuler.Parent := P_VRuler;
    VRuler.Top := 0;
    VRuler.Width := P_VRuler.Width;}
end;

procedure TJvgReportEditorForm.Edit1Change(Sender: TObject);
begin
  Memo1.Text := TMemo(Sender).Text;
  if Assigned(SelectedControl) then
    SelectedControl.Text := TMemo(Sender).Text;
end;

procedure TJvgReportEditorForm.Memo1Change(Sender: TObject);
begin
  Edit1.Text := TMemo(Sender).Text;
  if Assigned(SelectedControl) then
    SelectedControl.Text := TMemo(Sender).Text;
end;

procedure TJvgReportEditorForm.SB_LeftClick(Sender: TObject);
begin
  if Assigned(SelectedControl) then
    with SelectedControl do
      case TControl(Sender).Tag of
        1: SideLeft := 1 - SideLeft;
        2: SideRight := 1 - SideRight;
        3: SideTop := 1 - SideTop;
        4: SideBottom := 1 - SideBottom;
        5:
          begin
            SideLeft := 0;
            SideTop := 0;
            SideRight := 0;
            SideBottom := 0;
            SB_Left.Down := false;
            SB_Top.Down := false;
            SB_Right.Down := false;
            SB_Bottom.Down := false;
          end;
      else
        begin
          SideLeft := 1;
          SideTop := 1;
          SideRight := 1;
          SideBottom := 1;
          SB_Left.Down := true;
          SB_Top.Down := true;
          SB_Right.Down := true;
          SB_Bottom.Down := true;
        end;
      end;
end;

procedure TJvgReportEditorForm.FontComboBox1Change(Sender: TObject);
begin
  if not Assigned(SelectedControl) then
    exit;
  //-----!  SelectedControl.FName := TFontComboBox(Sender).FontName;
end;

procedure TJvgReportEditorForm.RxSpinEdit1Change(Sender: TObject);
begin
  if not Assigned(SelectedControl) then
    exit;
  SelectedControl.FSize := trunc(TSpinEdit(Sender).Value);
end;

procedure TJvgReportEditorForm.ColorComboBox1Change(Sender: TObject);
begin
  if not Assigned(SelectedControl) then
    exit;
  //-----!  SelectedControl.FColor := TColorComboBox(Sender).ColorValue;
end;

procedure TJvgReportEditorForm.FormShow(Sender: TObject);
begin
  Component.OwnerWnd := self;
  Component.ParentWnd := ScrollBox;
  if Component.FReportList.Count > 0 then
    Component.CreateReport(ScrollBox, true);
  //  Randomize;
  UpdatePageSize;
  AssignEventsToAllComponents;
  RemakeComponentsList;
  //  ShapeSize.Width := Printer.PageWidth;//f_PrintReport.CBReport1.Width;
  //  ShapeSize.Height := Printer.PageHeight;//f_PrintReport.CBReport1.Height;
end;

procedure TJvgReportEditorForm.sb_BookClick(Sender: TObject);
begin
  {  if TControl(Sender).Tag = 1 then
      f_PrintReport.CBReport1.Orientation := f_PrintReport.PrintWin1.Orientation
      else  f_PrintReport.CBReport1.Orientation := f_PrintReport.PrintWin2.Orientation;}
  if TControl(Sender).Tag = 1 then
    Printer.Orientation := poPortrait
  else
    Printer.Orientation := poLandscape;
  UpdatePageSize;
end;

procedure TJvgReportEditorForm.N1Click(Sender: TObject);
begin
  ScrollBox.RemoveControl(SelectedControl);
  ScrollBox.InsertControl(SelectedControl);
end;

procedure TJvgReportEditorForm.sb_FontBoldClick(Sender: TObject);
begin
  if not Assigned(SelectedControl) then
    exit;
  with SelectedControl do
    case TControl(Sender).Tag of
      1: FStyle := FStyle xor 1;
      2: FStyle := FStyle xor 2;
      3: FStyle := FStyle xor 4;
    end;
end;

procedure TJvgReportEditorForm.sb_AlignLClick(Sender: TObject);
begin
  if not Assigned(SelectedControl) then
    exit;
  SelectedControl.Alignment := TControl(Sender).Tag;
end;

procedure TJvgReportEditorForm.sbFontColorClick(Sender: TObject);
var
  i: integer;
begin
  if not Assigned(SelectedControl) then
    exit;
  with ColorDialog1 do
  begin
    case TControl(Sender).Tag of
      0: Color := SelectedControl.FColor;
      1: Color := SelectedControl.BkColor;
    else
      Color := SelectedControl.BvColor;
    end;

    if Execute then
      for i := 0 to ScrollBox.ControlCount - 1 do
        if ScrollBox.Controls[i] is TJvgReportItem then
          with TJvgReportItem(ScrollBox.Controls[i]) do
            if TJvgReportItem(ScrollBox.Controls[i]).Selected then
              case TControl(Sender).Tag of
                0: TJvgReportItem(ScrollBox.Controls[i]).FColor :=
                  Color;
                1: TJvgReportItem(ScrollBox.Controls[i]).BkColor :=
                  Color;
              else
                TJvgReportItem(ScrollBox.Controls[i]).BvColor := Color;
              end;
  end;
end;

procedure TJvgReportEditorForm.N_DeleteObjectClick(Sender: TObject);
begin
  if Assigned(SelectedControl) then
  begin
    if Windows.MessageBox(0, 'Delete object?', 'Confirm', MB_OKCANCEL) <> IDOK then
      exit;

    if SelectedControl.ContainOLE then
      ScrollBox.RemoveControl(SelectedControl.OLEContainer);
    ScrollBox.RemoveControl(SelectedControl);
    SelectedControl.Free;
    SelectedControl := nil;
    RemakeComponentsList;
  end;
end;

procedure TJvgReportEditorForm.OnDrawScrollBox(Sender: TObject);
begin
  VRuler.Top := ShapeSize.Top;
  HRuler.Left := ShapeSize.Left + P_VRuler.Width;
end;

procedure TJvgReportEditorForm.FormDestroy(Sender: TObject);
begin
  //...
end;

procedure TJvgReportEditorForm.RemakeComponentsList;
var
  i: integer;
begin
  cb_Components.Items.Clear;
  for i := 0 to ScrollBox.ControlCount - 1 do
    if ScrollBox.Controls[i] is TJvgReportItem then
      cb_Components.Items.Add(TJvgReportItem(ScrollBox.Controls[i]).CompName);
  cb_Components.Text := '';
  lb_Params.Items.Clear;
  for i := 0 to Component.ParamNames.Count - 1 do
    lb_Params.Items.Add(Component.ParamNames[i]);
end;

procedure TJvgReportEditorForm.UpdatePageSize;
const
  Sizes: array[boolean, 1..2] of integer = ((21, 29),
    (29, 21));
begin
  ShapeSize.Width := round(Sizes[Printer.Orientation = poLandscape][1] *
    GetDeviceCaps(Canvas.Handle, LOGPIXELSX) * 1.541 * 2.54 / 10);
  ShapeSize.Height := round(Sizes[Printer.Orientation = poLandscape][2] *
    GetDeviceCaps(Canvas.Handle, LOGPIXELSY) * 1.541 * 2.54 / 10);
  HRuler.Width := ShapeSize.Width + 10;
  VRuler.Height := ShapeSize.Height + 10;
end;

procedure TJvgReportEditorForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  l, t, w, h: integer;
begin
  w := 0;
  h := 0;
  if (Shift = [ssCtrl]) and (chr(Key) = 'Z') and fCanUndo then
  begin
    fCanUndo := false;
    ResizeReportControls(UndoPosShift.X, UndoPosShift.Y, 0, 0, true);
  end;

  if Assigned(ActiveControl) then
    exit;
  l := 0;
  t := 0;
  case Key of
    VK_UP: if Shift = [ssShift] then
        h := -1
      else if Shift = [ssCtrl] then
        t := -1;
    VK_DOWN: if Shift = [ssShift] then
        h := 1
      else if Shift = [ssCtrl] then
        t := 1;
    VK_LEFT: if Shift = [ssShift] then
        w := -1
      else if Shift = [ssCtrl] then
        l := -1;
    VK_RIGHT: if Shift = [ssShift] then
        w := 1
      else if Shift = [ssCtrl] then
        l := 1;
  else
    exit;
  end;
  ResizeReportControls(l, t, w, h, true);

end;

procedure TJvgReportEditorForm.sb_FixMovingClick(Sender: TObject);
var
  i: integer;
begin
  with ScrollBox do
    for i := 0 to ControlCount - 1 do
      if (Controls[i] is TJvgReportItem) and
        TJvgReportItem(Controls[i]).Selected then
        TJvgReportItem(Controls[i]).Fixed := integer(sb_FixMoving.Down);
end;

procedure TJvgReportEditorForm.N_AutoSizeClick(Sender: TObject);
begin
  SelectedControl.OLESizeMode := TMenuItem(Sender).Tag;
end;

procedure TJvgReportEditorForm.sb_SnapToGridClick(Sender: TObject);
begin
  if sb_SnapToGrid.Down then
  begin
    Step.X := Grid.X;
    Step.Y := Grid.Y;
  end
  else
  begin
    Step.X := 1;
    Step.Y := 1;
  end;
end;

procedure TJvgReportEditorForm.se_SizeChange(Sender: TObject);
begin
  if (not fMouseDown) and not fSkipSizeUpdate then
    ResizeReportControls(se_Left.Value, IGNORE_VALUE,
      IGNORE_VALUE, IGNORE_VALUE,
      false {fUseParamsAsShifts});
  ShowComponentPos(SelectedControl);
  fSkipSizeUpdate := false;
end;

procedure TJvgReportEditorForm.se_TopChange(Sender: TObject);
begin
  if (not fMouseDown) and not fSkipSizeUpdate then
    ResizeReportControls(IGNORE_VALUE, se_Top.Value,
      IGNORE_VALUE, IGNORE_VALUE,
      false {fUseParamsAsShifts});
  ShowComponentPos(SelectedControl);
  fSkipSizeUpdate := false;
end;

procedure TJvgReportEditorForm.se_WidthChange(Sender: TObject);
begin
  if (not fMouseDown) and not fSkipSizeUpdate then
    ResizeReportControls(IGNORE_VALUE, IGNORE_VALUE,
      se_Width.Value, IGNORE_VALUE,
      false {fUseParamsAsShifts});
  ShowComponentPos(SelectedControl);
  fSkipSizeUpdate := false;
end;

procedure TJvgReportEditorForm.se_HeightChange(Sender: TObject);
begin
  if (not fMouseDown) and not fSkipSizeUpdate then
    ResizeReportControls(IGNORE_VALUE, IGNORE_VALUE,
      IGNORE_VALUE, se_Height.Value,
      false {fUseParamsAsShifts});
  ShowComponentPos(SelectedControl);
  fSkipSizeUpdate := false;
end;

procedure TJvgReportEditorForm.ResizeReportControls(l, t, w, h: integer;
  fUseParamsAsShifts: boolean);
var
  i: integer;
begin
  with ScrollBox do
    for i := 0 to ControlCount - 1 do
      if (Controls[i] is TJvgReportItem) then
        with TJvgReportItem(Controls[i]) do
          if Selected and not bool(Fixed) then
            if fUseParamsAsShifts then
            begin
              if l < IGNORE_VALUE then
                Left := Left + l;
              if t < IGNORE_VALUE then
                Top := Top + t;
              if w < IGNORE_VALUE then
                Width := Width + w;
              if h < IGNORE_VALUE then
                Height := Height + h;
            end
            else
            begin
              if l < IGNORE_VALUE then
                Left := l;
              if t < IGNORE_VALUE then
                Top := t;
              if w < IGNORE_VALUE then
                Width := w;
              if h < IGNORE_VALUE then
                Height := h;
            end;
end;

procedure TJvgReportEditorForm.cb_ComponentsChange(Sender: TObject);
var
  i: integer;
begin
  if Assigned(SelectedControl) then
    if SelectedControl.CompName = cb_Components.Text then
      exit;
  with ScrollBox do
    for i := 0 to ControlCount - 1 do
      if (Controls[i] is TJvgReportItem) then
        if TJvgReportItem(Controls[i]).CompName = cb_Components.Text then
        begin
          OnMouseDown_(Controls[i], mbLeft, [], 0, 0);
          OnMouseUp_(Controls[i], mbLeft, [], 0, 0);
          exit;
        end;
end;

procedure TJvgReportEditorForm.ShowComponentPos(Control: TControl);
begin
  if Component = nil then
    exit;
  se_Left.Value := Control.Left;
  se_Top.Value := Control.Top;
  se_Width.Value := Control.Width;
  se_Height.Value := Control.Height;
end;

procedure TJvgReportEditorForm.AssignEventsToAllComponents;
var
  i: integer;
begin
  with Component.ParentWnd do
    for i := 0 to ControlCount - 1 do
      if (Controls[i] is TJvgReportItem) then
        with TJvgReportItem(Controls[i]) do
        begin
          OnMouseDown := OnMouseDown_;
          OnMouseUp := OnMouseUp_;
          OnMouseMove := OnMouseMove_;
          OnResize := OnResize_;
        end;
end;

function CanAlignControl(Control: TControl): boolean;
begin
  Result := (Control is TJvgReportItem) and
    (bool(TJvgReportItem(Control).Selected))
    and (not bool(TJvgReportItem(Control).Fixed));
end;

procedure TJvgReportEditorForm.N4Click(Sender: TObject);
begin
  if not Assigned(AlignForm) then
    AlignForm := TAlignForm.Create(nil);
  if AlignForm.ShowModal = mrOK then
    AlignControlsInWindow(Component.ParentWnd, CanAlignControl,
      AlignForm.Horz, AlignForm.Vert);
end;

procedure TJvgReportEditorForm.sb_BevelBoldClick(Sender: TObject);
var
  i: integer;
begin
  with Component.ParentWnd do
    for i := 0 to ControlCount - 1 do
      if (Controls[i] is TJvgReportItem) and
        bool(TJvgReportItem(Controls[i]).Selected) then
        TJvgReportItem(Controls[i]).PenWidth := 1 +
          integer(sb_BevelBold.Down);

end;

procedure TJvgReportEditorForm.UpdateToolBar(Control: TJvgReportItem);
begin
  with Control do
  begin
    {
      se_Left.Enabled := Assigned(Control);
      se_Top.Enabled := Assigned(Control);
      se_Width.Enabled := Assigned(Control);
      se_Height.Enabled := Assigned(Control);}
    P_Sides.Enabled := Assigned(Control);
    P_Font.Enabled := Assigned(Control);
    P_SBar.Enabled := Assigned(Control);
    if not Assigned(Control) then
      exit;
    Edit1.Text := Text;
    Memo1.Text := Text;
    FontComboBox1.Text := FName;
    RxSpinEdit1.Value := FSize;
    ColorComboBox1.Color := FColor;

    SB_Left.Down := SideLeft = 1;
    SB_Top.Down := SideTop = 1;
    SB_Right.Down := SideRight = 1;
    SB_Bottom.Down := SideBottom = 1;
    case Alignment of
      1: sb_AlignL.Down := true;
      2: sb_AlignR.Down := true;
      3: sb_AlignC.Down := true;
      4: sb_AlignW.Down := true;
    end;
    sb_FixMoving.Down := bool(Fixed);
    sb_FontBold.Down := boolean(FStyle and 1);
    sb_FontItalic.Down := boolean(FStyle and 2);
    sb_FontUnderline.Down := boolean(FStyle and 4);
    FE_OLE.Text := OLELinkToFile;
    FE_OLE.Enabled := ContainOLE;
    sb_BevelBold.Down := bool(PenWidth - 1);
    fSkipSizeUpdate := true;
    ShowComponentPos(Control);

    cb_Components.Text := CompName;

  end;
end;

procedure TJvgReportEditorForm.FormClose(Sender: TObject; var Action:
  TCloseAction);
var
  msS, msT: TMemoryStream;
begin
  if Assigned(AlignForm) then
    AlignForm.Free;
  if Assigned(ReportParamEditor) then
    ReportParamEditor.Free;
  ScrollBox.HorzScrollBar.Position := 0;
  ScrollBox.VertScrollBar.Position := 0;
  Component.Save;
end;

procedure TJvgReportEditorForm.FE_OLEChange(Sender: TObject);
var
  str: string;
begin
  if (not Assigned(SelectedControl)) or (not FileExists(FE_OLE.Text)) then
    exit;
  str := FE_OLE.Text;
  if ExtractFilePath(Name) = ExtractFilePath(ParamStr(0)) then
    str := ExtractFileName(Name);
  if SelectedControl.OLELinkToFile <> str then
    SelectedControl.OLELinkToFile := str;
end;

procedure TJvgReportEditorForm.SpeedButton1Click(Sender: TObject);
begin
  if OpenOLEFile.Execute then
    FE_OLE.Text := OpenOLEFile.FileName;
end;

procedure TJvgReportEditorForm.sb_PrintClick(Sender: TObject);
begin
  if Assigned(Component) and (Component.ComponentList.Count > 0) then
    Component.Print;
  Component.OwnerWnd := self;
  Component.ParentWnd := ScrollBox;
end;

procedure TJvgReportEditorForm.sb_PreviewClick(Sender: TObject);
var
  Form: TForm;
  Image: TImage;
  bmp: TBitmap;
  R: TRect;
  i, W, H: integer;
begin
  if not Assigned(Component) then
    exit;
  Form := TForm.Create(nil);
  Form.Caption := 'Page Preview';
  Image := TImage.Create(Form);
  bmp := TBitmap.Create;
  Image.Parent := Form;
  H := SantimsToPixels(Form.Canvas.Handle, 29, true);
  W := SantimsToPixels(Form.Canvas.Handle, 21, false);
  //  Image.Width := W+8;
  //  Image.Height := H+8;
  Image.Left := 0;
  Image.Top := 0;
  bmp.Width := W + 7;
  bmp.Height := H + 7;
  try
    //    bmp.Canvas.Brush.Color := clWhite;
    //    R := Image.ClientRect; bmp.Canvas.FillRect( R );

    with Component do
      for i := 0 to ComponentList.Count - 1 do
        with TJvgReportItem(ComponentList[i]) do
        begin
          PaintTo(bmp.Canvas);
          if ContainOle then
            OLEContainer.PaintTo(bmp.Canvas.Handle, Left, Top);
        end;

    bmp.Canvas.Brush.Color := clBtnFace;
    R := Bounds(bmp.Width - 7, 0, 7, bmp.Height - 7);
    bmp.Canvas.FillRect(R);
    R := Bounds(0, bmp.Height - 7, bmp.Width - 7, 7);
    bmp.Canvas.FillRect(R);
    bmp.Canvas.Brush.Color := 0;
    R := Bounds(bmp.Width - 7, 7, 7, bmp.Height - 7);
    bmp.Canvas.FillRect(R);
    R := Bounds(7, bmp.Height - 7, bmp.Width - 7, 7);
    bmp.Canvas.FillRect(R);

    Image.Picture.bitmap := bmp;
    Image.Stretch := true;
    Image.Width := W div 2;
    Image.Height := H div 2;
    Form.ClientWidth := Image.Width;
    Form.ClientHeight := Image.Height;

    bmp.Free;
    bmp := nil;
    Form.ShowModal;
  finally
    if Assigned(bmp) then
      bmp.Free;
    Form.Free;
  end;
end;

procedure TJvgReportEditorForm.SpeedButton2Click(Sender: TObject);
begin
  if not Assigned(ReportParamEditor) then
    ReportParamEditor := TJvgReportParamEditor.Create(nil);
  ReportParamEditor.ShowModal;
end;

procedure TJvgReportEditorForm.cb_ComponentsKeyDown(Sender: TObject; var Key:
  Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) then
  begin
    SelectedControl.CompName := trim(cb_Components.Text);
    RemakeComponentsList;
    cb_Components.Text := SelectedControl.CompName;
  end;
  if (Key = VK_ESCAPE) then
    cb_Components.Text := SelectedControl.CompName;

end;

procedure TJvgReportEditorForm.CheckBox1Click(Sender: TObject);
begin
  if Assigned(SelectedControl) then
    SelectedControl.Transparent := integer(TCheckBox(Sender).Checked);
end;

end.
