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

UNIT JvgReportEditor;

INTERFACE

USES
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
   graphics{$IFDEF COMPILER5_UP},
   Imglist{$ENDIF};

TYPE

   TJvgRepProperty = CLASS(TPropertyEditor)
      FUNCTION GetAttributes: TPropertyAttributes; OVERRIDE;
      FUNCTION GetValue: STRING; OVERRIDE;
      PROCEDURE Edit; OVERRIDE;
   END;

   TJvgReportCompEditor = CLASS(TComponentEditor)
      PROCEDURE ExecuteVerb(Index: Integer); OVERRIDE;
      FUNCTION GetVerb(Index: Integer): STRING; OVERRIDE;
      FUNCTION GetVerbCount: Integer; OVERRIDE;
   END;

   TJvgReportEditor = CLASS(TComponent)
      FReport: TJvgReport;
   PROTECTED
      PROCEDURE Notification(AComponent: TComponent; Operation: TOperation);
         OVERRIDE;
   PUBLIC
      PROCEDURE Preview;
      PROCEDURE Edit;
   PUBLISHED
      PROPERTY Report: TJvgReport READ FReport WRITE FReport;
   END;

   TJvgReportEditorForm = CLASS(TForm)
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
      PROCEDURE OpenClick(Sender: TObject);
      PROCEDURE Save1Click(Sender: TObject);
      PROCEDURE ScrollBox_MouseDown(Sender: TObject; Button: TMouseButton;
         Shift: TShiftState; X, Y: Integer);
      PROCEDURE FormCreate(Sender: TObject);
      PROCEDURE Edit1Change(Sender: TObject);
      PROCEDURE SB_LeftClick(Sender: TObject);
      PROCEDURE FontComboBox1Change(Sender: TObject);
      PROCEDURE RxSpinEdit1Change(Sender: TObject);
      PROCEDURE ColorComboBox1Change(Sender: TObject);
      PROCEDURE FormShow(Sender: TObject);
      PROCEDURE sb_BookClick(Sender: TObject);
      PROCEDURE N1Click(Sender: TObject);
      PROCEDURE sb_FontBoldClick(Sender: TObject);
      PROCEDURE sb_AlignLClick(Sender: TObject);
      PROCEDURE Memo1Change(Sender: TObject);
      PROCEDURE sbFontColorClick(Sender: TObject);
      PROCEDURE N_DeleteObjectClick(Sender: TObject);
      PROCEDURE ScrollBox_MouseMove(Sender: TObject; Shift: TShiftState; X,
         Y: Integer);
      PROCEDURE ScrollBox_MouseUp(Sender: TObject; Button: TMouseButton;
         Shift: TShiftState; X, Y: Integer);
      PROCEDURE FormDestroy(Sender: TObject);
      PROCEDURE FormKeyDown(Sender: TObject; VAR Key: Word;
         Shift: TShiftState);
      PROCEDURE sb_FixMovingClick(Sender: TObject);
      PROCEDURE N_AutoSizeClick(Sender: TObject);
      PROCEDURE sb_SnapToGridClick(Sender: TObject);
      PROCEDURE se_SizeChange(Sender: TObject);
      PROCEDURE cb_ComponentsChange(Sender: TObject);
      PROCEDURE N4Click(Sender: TObject);
      PROCEDURE sb_BevelBoldClick(Sender: TObject);
      PROCEDURE se_TopChange(Sender: TObject);
      PROCEDURE se_WidthChange(Sender: TObject);
      PROCEDURE se_HeightChange(Sender: TObject);
      PROCEDURE FormClose(Sender: TObject; VAR Action: TCloseAction);
      PROCEDURE FE_OLEChange(Sender: TObject);
      PROCEDURE SpeedButton1Click(Sender: TObject);
      PROCEDURE sb_PrintClick(Sender: TObject);
      PROCEDURE sb_PreviewClick(Sender: TObject);
      PROCEDURE SpeedButton2Click(Sender: TObject);
      PROCEDURE cb_ComponentsKeyDown(Sender: TObject; VAR Key: Word;
         Shift: TShiftState);
      PROCEDURE CheckBox1Click(Sender: TObject);
   PRIVATE
      ReportParamEditor: TJvgReportParamEditor;

      fMouseDown, fCanUndo, fSkipSizeUpdate: boolean;
      UndoPosShift, SelectedControlLastPos: TPoint;
      ControlPos, SelPt, Grid, Step: TPoint;
      ScrollBox: TJvgRepScrollBox;
      SelectedControl: TJvgReportItem;
      fSelection: boolean;
      SelectionRect: TRect;
      PROCEDURE RemakeComponentsList;
      PROCEDURE read(FileName: STRING; ParentWnd: TWinControl);
      PROCEDURE Save(FileName: STRING);
      PROCEDURE OnMouseDown_(Sender: TObject; Button: TMouseButton; Shift:
         TShiftState; X, Y: Integer);
      PROCEDURE OnMouseUp_(Sender: TObject; Button: TMouseButton; Shift:
         TShiftState; X, Y: Integer);
      PROCEDURE OnMouseMove_(Sender: TObject; Shift: TShiftState; X, Y:
         Integer);
      PROCEDURE OnResize_(Sender: TObject);
      PROCEDURE OnDrawScrollBox(Sender: TObject);
      PROCEDURE UpdatePageSize;
      PROCEDURE ResizeReportControls(l, t, w, h: integer; fUseParamsAsShifts:
         boolean);
      PROCEDURE ShowComponentPos(Control: TControl);
      PROCEDURE AssignEventsToAllComponents;
      PROCEDURE UpdateToolBar(Control: TJvgReportItem);
   PUBLIC
      Component: TJvgReport;
      PROCEDURE Preview(JvgReport: TJvgReport);
      PROCEDURE Edit(JvgReport: TJvgReport);
   END;

   TJvgPublicControl = CLASS(TControl)
   PUBLIC
      PROPERTY Caption;
   END;

   TJvgPublicControlClass = CLASS OF TJvgPublicControl;

CONST
   IGNORE_VALUE               = 65536;
VAR
   glRepEditor                : TJvgReportEditorForm;
   Form2                      : TComponent;

IMPLEMENTATION
USES JvgTypes,
   JvgUtils, {PrintR,}                  { ConfirmF, AboutF,}
   JvgAlignFunction,
   JvgAlignForm;
{$R *.DFM}
//----------- common proc

PROCEDURE ShowReportEditor(JvgReport: TComponent; fEdit: boolean);
VAR
   Dialog                     : TJvgReportEditorForm;
   Report                     : TJvgReport;
BEGIN
   Dialog := TJvgReportEditorForm.Create(Application);
   IF JvgReport IS TJvgReport THEN
      Report := TJvgReport(JvgReport)
   ELSE
      Report := TJvgReportEditor(JvgReport).Report;
   IF Report = NIL THEN
      exit;
   IF fEdit THEN
      Dialog.Edit(Report)
   ELSE
      Dialog.Preview(Report);
   Dialog.free;
END;

//----------- TJvgComponentListProperty

FUNCTION TJvgRepProperty.GetAttributes: TPropertyAttributes;
BEGIN
   Result := [paDialog];
END;

FUNCTION TJvgRepProperty.GetValue: STRING;
BEGIN
   Result := Format('(%s)', [GetPropType^.Name]);
END;

PROCEDURE TJvgRepProperty.Edit;
BEGIN
   ShowReportEditor(TJvgReport(GetComponent(0)), true);
END;
//----------- TJvgReportCompEditor

PROCEDURE TJvgReportCompEditor.ExecuteVerb(Index: Integer);
BEGIN
   CASE Index OF
      0: ShowReportEditor(Component, true);
      1: ShowReportEditor(Component, false);
   END;
END;

FUNCTION TJvgReportCompEditor.GetVerb(Index: Integer): STRING;
BEGIN
   CASE Index OF
      0: Result := 'Edit report...';
      1: Result := 'Preview report...';
   END;
END;

FUNCTION TJvgReportCompEditor.GetVerbCount: Integer;
BEGIN
   Result := 2;
END;
//----------- TJvgReportEditor

PROCEDURE TJvgReportEditor.Notification(AComponent: TComponent; Operation:
   TOperation);
BEGIN
   IF (AComponent = Report) AND (Operation = opRemove) THEN
      Report := NIL;
   INHERITED;
END;

PROCEDURE TJvgReportEditor.Preview;
BEGIN
   ShowReportEditor(self, false);
END;

PROCEDURE TJvgReportEditor.Edit;
BEGIN
   ShowReportEditor(self, true);
END;
//----------- TJvgReportEditorForm

PROCEDURE TJvgReportEditorForm.Preview(JvgReport: TJvgReport);
BEGIN
   PC.Visible := false;
   ScrollBox.Enabled := false;
   Component := JvgReport;
   ShowModal;
END;

PROCEDURE TJvgReportEditorForm.Edit(JvgReport: TJvgReport);
BEGIN
   PC.Visible := true;
   ScrollBox.Enabled := true;
   Component := JvgReport;
   ShowModal;
END;

PROCEDURE TJvgReportEditorForm.OnMouseDown_(Sender: TObject; Button:
   TMouseButton; Shift: TShiftState; X, Y: Integer);
VAR
   R                          : TRect;
   pt                         : TPoint;
   i                          : integer;
BEGIN
   IF TControl(Sender).Cursor <> crDefault THEN
      exit;
   ActiveControl := NIL;
   fCanUndo := false;
   IF (B_Label.Down) OR (B_Bevel.Down) OR (sb_OLE.Down) OR (ssCtrl IN Shift)
      THEN
   BEGIN
      ScrollBox_MouseDown(Sender, Button, Shift, X + TControl(Sender).Left, Y +
         TControl(Sender).Top);
      exit;
   END;
   ScrollBox_MouseDown(Sender, Button, Shift, X, Y);
   IF Button = mbLeft THEN
   BEGIN
      fMouseDown := true;
      TControl(Sender).Tag := 1;
      ControlPos.x := X;
      ControlPos.y := Y;
   END
   ELSE IF Button = mbRight THEN
   BEGIN
      OnMouseDown_(Sender, mbLeft, Shift, X, Y);
      pt.x := X;
      pt.y := Y;
      pt := SelectedControl.ClientToScreen(pt);
      N_Linktofile.Enabled := SelectedControl.ContainOLE;
      N_OLESize.Enabled := SelectedControl.ContainOLE;
      CASE SelectedControl.OLESizeMode OF
         0: N_Clip.Checked := true;
         1: N_Center.Checked := true;
         2: N_Scale.Checked := true;
         3: N_Stretch.Checked := true;
         4: N_AutoSize.Checked := true;
      END;

      PM_Control.Popup(pt.X, pt.Y);
      OnMouseUp_(Sender, Button, Shift, X, Y);
   END;

   IF Assigned(SelectedControl) THEN
      IF ssShift IN Shift THEN
      BEGIN
         TJvgReportItem(Sender).Selected := NOT TJvgReportItem(Sender).Selected;
         fMouseDown := false;
         IF SelectedControl = Sender THEN
            exit;
      END
      ELSE
      BEGIN
         IF TJvgReportItem(Sender).Selected THEN
            exit;
         WITH ScrollBox DO
            FOR i := 0 TO ControlCount - 1 DO
               IF (Controls[i] IS TJvgReportItem) AND
                  TJvgReportItem(Controls[i]).Selected THEN
                  TJvgReportItem(Controls[i]).Selected := false;
         SelectedControl.Selected := false;
         SelectedControl.Invalidate;
      END;

   SelectedControl := TJvgReportItem(Sender);
   SelectedControl.Selected := true;

   UpdateToolBar(SelectedControl);
   SelectedControlLastPos.X := SelectedControl.Left;
   SelectedControlLastPos.Y := SelectedControl.Top;

END;

PROCEDURE TJvgReportEditorForm.OnMouseUp_(Sender: TObject; Button: TMouseButton;
   Shift: TShiftState; X, Y: Integer);
VAR
   R                          : TRect;
BEGIN
   ScrollBox_MouseUp(Sender, Button, Shift, X, Y);
   fMouseDown := false;
   IF NOT Assigned(SelectedControl) THEN
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
END;

PROCEDURE TJvgReportEditorForm.OnMouseMove_(Sender: TObject; Shift: TShiftState;
   X, Y: Integer);
VAR
   DC                         : HDC;
   i                          : integer;
BEGIN
   IF fSelection THEN
      ScrollBox_MouseMove(Sender, Shift, X, Y);
   IF sb_FixAllMoving.Down THEN
      exit;
   //  if TControl(Sender).Tag = 0 then exit;
   IF NOT fMouseDown THEN
      exit;
   WITH ScrollBox DO
      FOR i := 0 TO ControlCount - 1 DO
         IF (Controls[i] IS TJvgReportItem) THEN
            WITH TJvgReportItem(Controls[i]) DO
               IF Selected AND NOT bool(Fixed) THEN
               BEGIN
                  Left := ((Left + X - ControlPos.x) DIV Step.X) * Step.X;
                  Top := ((Top + Y - ControlPos.y) DIV Step.Y) * Step.Y;
               END;
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
END;

PROCEDURE TJvgReportEditorForm.OnResize_(Sender: TObject);
BEGIN
   fSkipSizeUpdate := true;
   IF Sender = SelectedControl THEN
      ShowComponentPos(TControl(Sender));
END;

PROCEDURE TJvgReportEditorForm.read(FileName: STRING; ParentWnd: TWinControl);
BEGIN
   ScrollBox.HorzScrollBar.Position := 0;
   ScrollBox.VertScrollBar.Position := 0;
   SelectedControl := NIL;
   UpdateToolBar(NIL);
   Component.LoadFromFile(FileName);
   Component.CreateReport(ParentWnd, true);
   AssignEventsToAllComponents;
   RemakeComponentsList;
END;

PROCEDURE TJvgReportEditorForm.Save(FileName: STRING);
BEGIN
   ScrollBox.HorzScrollBar.Position := 0;
   ScrollBox.VertScrollBar.Position := 0;
   Component.SaveToFile(FileName);
END;

PROCEDURE TJvgReportEditorForm.OpenClick(Sender: TObject);
BEGIN
   OpenDialog1.InitialDir := ExtractFilePath(ParamStr(0));
   IF OpenDialog1.Execute THEN
      Read(OpenDialog1.FileName, ScrollBox);
END;

PROCEDURE TJvgReportEditorForm.Save1Click(Sender: TObject);
BEGIN
   SaveDialog1.InitialDir := ExtractFilePath(ParamStr(0));
   IF SaveDialog1.Execute THEN
      Save(SaveDialog1.FileName);
END;

PROCEDURE TJvgReportEditorForm.ScrollBox_MouseDown(Sender: TObject; Button:
   TMouseButton;
   Shift: TShiftState; X, Y: Integer);
VAR
   l, Compon                  : TJvgReportItem;
   R                          : TRect;
   pt                         : TPoint;
BEGIN
   IF ssCtrl IN Shift THEN
   BEGIN
      SelectionRect := Rect(0, 0, 0, 0);
      SelPt.X := X - ScrollBox.HorzScrollBar.Position;
      SelPt.Y := Y - ScrollBox.VertScrollBar.Position;
      SelPt := ScrollBox.ClientToScreen(SelPt);
      fSelection := true;
   END;
   IF (B_Label.Down) OR (B_Bevel.Down) OR (sb_OLE.Down) THEN
   BEGIN
      Compon := Component.AddComponent;
      WITH Compon DO
      BEGIN
         Left := X - ScrollBox.HorzScrollBar.Position;
         Top := Y - ScrollBox.VertScrollBar.Position;
         IF B_Label.Down THEN
         BEGIN
            Text := 'Label';
            SideLeft := 0;
            SideTop := 0;
            SideRight := 0;
            SideBottom := 0;
         END;
         OnMouseDown := OnMouseDown_;
         OnMouseUp := OnMouseUp_;
         OnMouseMove := OnMouseMove_;
         OnResize := OnResize_;
         ContainOLE := sb_OLE.Down;
         B_Label.Down := false;
         B_Bevel.Down := false;
         sb_OLE.Down := false;
         RemakeComponentsList;
      END;
   END
   ELSE
   BEGIN
      R := ScrollBox.ClientRect;
      pt.x := 0;
      pt.y := 0;
      pt := ScrollBox.ClientToScreen(pt);
      OffsetRect(R, pt.x, pt.y);
      ClipCursor(@R);
   END;
END;

PROCEDURE TJvgReportEditorForm.ScrollBox_MouseMove(Sender: TObject; Shift:
   TShiftState;
   X, Y: Integer);
VAR
   DC                         : HDC;
   pt                         : TPoint;
BEGIN

   IF NOT fSelection THEN
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
END;

PROCEDURE TJvgReportEditorForm.ScrollBox_MouseUp(Sender: TObject; Button:
   TMouseButton; Shift: TShiftState; X, Y: Integer);
VAR
   DC                         : HDC;
   R                          : TRect;
   i                          : integer;
BEGIN
   IF fSelection THEN
   BEGIN
      DC := GetDC(0);
      DrawFocusRect(DC, SelectionRect);
      ReleaseDC(0, DC);
      fSelection := false;
      //...select all in rect
      WITH Component.ParentWnd DO
         FOR i := 0 TO ControlCount - 1 DO
            IF Controls[i] IS TJvgReportItem THEN
               WITH TJvgReportItem(Controls[i]) DO
               BEGIN
                  R := ClientRect;
                  OffsetRect(R, ClientOrigin.x, ClientOrigin.y);
                  IF IntersectRect(R, R, SelectionRect) THEN
                     Selected := true
                  ELSE
                     Selected := false;
               END;

   END;
   R := Rect(0, 0, Screen.Width, Screen.Height);
   ClipCursor(@R);
END;

PROCEDURE TJvgReportEditorForm.FormCreate(Sender: TObject);
VAR
   R                          : TRect;
BEGIN
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
END;

PROCEDURE TJvgReportEditorForm.Edit1Change(Sender: TObject);
BEGIN
   Memo1.Text := TMemo(Sender).Text;
   IF Assigned(SelectedControl) THEN
      SelectedControl.Text := TMemo(Sender).Text;
END;

PROCEDURE TJvgReportEditorForm.Memo1Change(Sender: TObject);
BEGIN
   Edit1.Text := TMemo(Sender).Text;
   IF Assigned(SelectedControl) THEN
      SelectedControl.Text := TMemo(Sender).Text;
END;

PROCEDURE TJvgReportEditorForm.SB_LeftClick(Sender: TObject);
BEGIN
   IF Assigned(SelectedControl) THEN
      WITH SelectedControl DO
         CASE TControl(Sender).Tag OF
            1: SideLeft := 1 - SideLeft;
            2: SideRight := 1 - SideRight;
            3: SideTop := 1 - SideTop;
            4: SideBottom := 1 - SideBottom;
            5:
               BEGIN
                  SideLeft := 0;
                  SideTop := 0;
                  SideRight := 0;
                  SideBottom := 0;
                  SB_Left.Down := false;
                  SB_Top.Down := false;
                  SB_Right.Down := false;
                  SB_Bottom.Down := false;
               END;
         ELSE
            BEGIN
               SideLeft := 1;
               SideTop := 1;
               SideRight := 1;
               SideBottom := 1;
               SB_Left.Down := true;
               SB_Top.Down := true;
               SB_Right.Down := true;
               SB_Bottom.Down := true;
            END;
         END;
END;

PROCEDURE TJvgReportEditorForm.FontComboBox1Change(Sender: TObject);
BEGIN
   IF NOT Assigned(SelectedControl) THEN
      exit;
   //-----!  SelectedControl.FName := TFontComboBox(Sender).FontName;
END;

PROCEDURE TJvgReportEditorForm.RxSpinEdit1Change(Sender: TObject);
BEGIN
   IF NOT Assigned(SelectedControl) THEN
      exit;
   SelectedControl.FSize := trunc(TSpinEdit(Sender).Value);
END;

PROCEDURE TJvgReportEditorForm.ColorComboBox1Change(Sender: TObject);
BEGIN
   IF NOT Assigned(SelectedControl) THEN
      exit;
   //-----!  SelectedControl.FColor := TColorComboBox(Sender).ColorValue;
END;

PROCEDURE TJvgReportEditorForm.FormShow(Sender: TObject);
BEGIN
   Component.OwnerWnd := self;
   Component.ParentWnd := ScrollBox;
   IF Component.FReportList.Count > 0 THEN
      Component.CreateReport(ScrollBox, true);
   //  Randomize;
   UpdatePageSize;
   AssignEventsToAllComponents;
   RemakeComponentsList;
   //  ShapeSize.Width := Printer.PageWidth;//f_PrintReport.CBReport1.Width;
   //  ShapeSize.Height := Printer.PageHeight;//f_PrintReport.CBReport1.Height;
END;

PROCEDURE TJvgReportEditorForm.sb_BookClick(Sender: TObject);
BEGIN
   {  if TControl(Sender).Tag = 1 then
       f_PrintReport.CBReport1.Orientation := f_PrintReport.PrintWin1.Orientation
       else  f_PrintReport.CBReport1.Orientation := f_PrintReport.PrintWin2.Orientation;}
   IF TControl(Sender).Tag = 1 THEN
      Printer.Orientation := poPortrait
   ELSE
      Printer.Orientation := poLandscape;
   UpdatePageSize;
END;

PROCEDURE TJvgReportEditorForm.N1Click(Sender: TObject);
BEGIN
   ScrollBox.RemoveControl(SelectedControl);
   ScrollBox.InsertControl(SelectedControl);
END;

PROCEDURE TJvgReportEditorForm.sb_FontBoldClick(Sender: TObject);
BEGIN
   IF NOT Assigned(SelectedControl) THEN
      exit;
   WITH SelectedControl DO
      CASE TControl(Sender).Tag OF
         1: FStyle := FStyle XOR 1;
         2: FStyle := FStyle XOR 2;
         3: FStyle := FStyle XOR 4;
      END;
END;

PROCEDURE TJvgReportEditorForm.sb_AlignLClick(Sender: TObject);
BEGIN
   IF NOT Assigned(SelectedControl) THEN
      exit;
   SelectedControl.Alignment := TControl(Sender).Tag;
END;

PROCEDURE TJvgReportEditorForm.sbFontColorClick(Sender: TObject);
VAR
   i                          : integer;
BEGIN
   IF NOT Assigned(SelectedControl) THEN
      exit;
   WITH ColorDialog1 DO
   BEGIN
      CASE TControl(Sender).Tag OF
         0: Color := SelectedControl.FColor;
         1: Color := SelectedControl.BkColor;
      ELSE
         Color := SelectedControl.BvColor;
      END;

      IF Execute THEN
         FOR i := 0 TO ScrollBox.ControlCount - 1 DO
            IF ScrollBox.Controls[i] IS TJvgReportItem THEN
               WITH TJvgReportItem(ScrollBox.Controls[i]) DO
                  IF TJvgReportItem(ScrollBox.Controls[i]).Selected THEN
                     CASE TControl(Sender).Tag OF
                        0: TJvgReportItem(ScrollBox.Controls[i]).FColor :=
                           Color;
                        1: TJvgReportItem(ScrollBox.Controls[i]).BkColor :=
                           Color;
                     ELSE
                        TJvgReportItem(ScrollBox.Controls[i]).BvColor := Color;
                     END;
   END;
END;

PROCEDURE TJvgReportEditorForm.N_DeleteObjectClick(Sender: TObject);
BEGIN
   IF Assigned(SelectedControl) THEN
   BEGIN
      IF Windows.MessageBox(0, 'Delete object?', 'Confirm', MB_OKCANCEL) <> IDOK
         THEN
         exit;

      IF SelectedControl.ContainOLE THEN
         ScrollBox.RemoveControl(SelectedControl.OLEContainer);
      ScrollBox.RemoveControl(SelectedControl);
      SelectedControl.Free;
      SelectedControl := NIL;
      RemakeComponentsList;
   END;
END;

PROCEDURE TJvgReportEditorForm.OnDrawScrollBox(Sender: TObject);
BEGIN
   VRuler.Top := ShapeSize.Top;
   HRuler.Left := ShapeSize.Left + P_VRuler.Width;
END;

PROCEDURE TJvgReportEditorForm.FormDestroy(Sender: TObject);
BEGIN
   //...
END;

PROCEDURE TJvgReportEditorForm.RemakeComponentsList;
VAR
   i                          : integer;
BEGIN
   cb_Components.Items.Clear;
   FOR i := 0 TO ScrollBox.ControlCount - 1 DO
      IF ScrollBox.Controls[i] IS TJvgReportItem THEN
         cb_Components.Items.Add(TJvgReportItem(ScrollBox.Controls[i]).CompName);
   cb_Components.Text := '';
   lb_Params.Items.Clear;
   FOR i := 0 TO Component.ParamNames.Count - 1 DO
      lb_Params.Items.Add(Component.ParamNames[i]);
END;

PROCEDURE TJvgReportEditorForm.UpdatePageSize;
CONST
   Sizes                      : ARRAY[boolean, 1..2] OF integer = ((21, 29),
      (29, 21));
BEGIN
   ShapeSize.Width := round(Sizes[Printer.Orientation = poLandscape][1] *
      GetDeviceCaps(Canvas.Handle, LOGPIXELSX) * 1.541 * 2.54 / 10);
   ShapeSize.Height := round(Sizes[Printer.Orientation = poLandscape][2] *
      GetDeviceCaps(Canvas.Handle, LOGPIXELSY) * 1.541 * 2.54 / 10);
   HRuler.Width := ShapeSize.Width + 10;
   VRuler.Height := ShapeSize.Height + 10;
END;

PROCEDURE TJvgReportEditorForm.FormKeyDown(Sender: TObject; VAR Key: Word;
   Shift: TShiftState);
VAR
   l, t, w, h                 : integer;
BEGIN
   w := 0;
   h := 0;
   IF (Shift = [ssCtrl]) AND (chr(Key) = 'Z') AND fCanUndo THEN
   BEGIN
      fCanUndo := false;
      ResizeReportControls(UndoPosShift.X, UndoPosShift.Y, 0, 0, true);
   END;

   IF Assigned(ActiveControl) THEN
      exit;
   l := 0;
   t := 0;
   CASE Key OF
      VK_UP: IF Shift = [ssShift] THEN
            h := -1
         ELSE IF Shift = [ssCtrl] THEN
            t := -1;
      VK_DOWN: IF Shift = [ssShift] THEN
            h := 1
         ELSE IF Shift = [ssCtrl] THEN
            t := 1;
      VK_LEFT: IF Shift = [ssShift] THEN
            w := -1
         ELSE IF Shift = [ssCtrl] THEN
            l := -1;
      VK_RIGHT: IF Shift = [ssShift] THEN
            w := 1
         ELSE IF Shift = [ssCtrl] THEN
            l := 1;
   ELSE
      exit;
   END;
   ResizeReportControls(l, t, w, h, true);

END;

PROCEDURE TJvgReportEditorForm.sb_FixMovingClick(Sender: TObject);
VAR
   i                          : integer;
BEGIN
   WITH ScrollBox DO
      FOR i := 0 TO ControlCount - 1 DO
         IF (Controls[i] IS TJvgReportItem) AND
            TJvgReportItem(Controls[i]).Selected THEN
            TJvgReportItem(Controls[i]).Fixed := integer(sb_FixMoving.Down);
END;

PROCEDURE TJvgReportEditorForm.N_AutoSizeClick(Sender: TObject);
BEGIN
   SelectedControl.OLESizeMode := TMenuItem(Sender).Tag;
END;

PROCEDURE TJvgReportEditorForm.sb_SnapToGridClick(Sender: TObject);
BEGIN
   IF sb_SnapToGrid.Down THEN
   BEGIN
      Step.X := Grid.X;
      Step.Y := Grid.Y;
   END
   ELSE
   BEGIN
      Step.X := 1;
      Step.Y := 1;
   END;
END;

PROCEDURE TJvgReportEditorForm.se_SizeChange(Sender: TObject);
BEGIN
   IF (NOT fMouseDown) AND NOT fSkipSizeUpdate THEN
      ResizeReportControls(se_Left.Value, IGNORE_VALUE,
         IGNORE_VALUE, IGNORE_VALUE,
         false {fUseParamsAsShifts});
   ShowComponentPos(SelectedControl);
   fSkipSizeUpdate := false;
END;

PROCEDURE TJvgReportEditorForm.se_TopChange(Sender: TObject);
BEGIN
   IF (NOT fMouseDown) AND NOT fSkipSizeUpdate THEN
      ResizeReportControls(IGNORE_VALUE, se_Top.Value,
         IGNORE_VALUE, IGNORE_VALUE,
         false {fUseParamsAsShifts});
   ShowComponentPos(SelectedControl);
   fSkipSizeUpdate := false;
END;

PROCEDURE TJvgReportEditorForm.se_WidthChange(Sender: TObject);
BEGIN
   IF (NOT fMouseDown) AND NOT fSkipSizeUpdate THEN
      ResizeReportControls(IGNORE_VALUE, IGNORE_VALUE,
         se_Width.Value, IGNORE_VALUE,
         false {fUseParamsAsShifts});
   ShowComponentPos(SelectedControl);
   fSkipSizeUpdate := false;
END;

PROCEDURE TJvgReportEditorForm.se_HeightChange(Sender: TObject);
BEGIN
   IF (NOT fMouseDown) AND NOT fSkipSizeUpdate THEN
      ResizeReportControls(IGNORE_VALUE, IGNORE_VALUE,
         IGNORE_VALUE, se_Height.Value,
         false {fUseParamsAsShifts});
   ShowComponentPos(SelectedControl);
   fSkipSizeUpdate := false;
END;

PROCEDURE TJvgReportEditorForm.ResizeReportControls(l, t, w, h: integer;
   fUseParamsAsShifts: boolean);
VAR
   i                          : integer;
BEGIN
   WITH ScrollBox DO
      FOR i := 0 TO ControlCount - 1 DO
         IF (Controls[i] IS TJvgReportItem) THEN
            WITH TJvgReportItem(Controls[i]) DO
               IF Selected AND NOT bool(Fixed) THEN
                  IF fUseParamsAsShifts THEN
                  BEGIN
                     IF l < IGNORE_VALUE THEN
                        Left := Left + l;
                     IF t < IGNORE_VALUE THEN
                        Top := Top + t;
                     IF w < IGNORE_VALUE THEN
                        Width := Width + w;
                     IF h < IGNORE_VALUE THEN
                        Height := Height + h;
                  END
                  ELSE
                  BEGIN
                     IF l < IGNORE_VALUE THEN
                        Left := l;
                     IF t < IGNORE_VALUE THEN
                        Top := t;
                     IF w < IGNORE_VALUE THEN
                        Width := w;
                     IF h < IGNORE_VALUE THEN
                        Height := h;
                  END;
END;

PROCEDURE TJvgReportEditorForm.cb_ComponentsChange(Sender: TObject);
VAR
   i                          : integer;
BEGIN
   IF Assigned(SelectedControl) THEN
      IF SelectedControl.CompName = cb_Components.Text THEN
         exit;
   WITH ScrollBox DO
      FOR i := 0 TO ControlCount - 1 DO
         IF (Controls[i] IS TJvgReportItem) THEN
            IF TJvgReportItem(Controls[i]).CompName = cb_Components.Text THEN
            BEGIN
               OnMouseDown_(Controls[i], mbLeft, [], 0, 0);
               OnMouseUp_(Controls[i], mbLeft, [], 0, 0);
               exit;
            END;
END;

PROCEDURE TJvgReportEditorForm.ShowComponentPos(Control: TControl);
BEGIN
   IF Component = NIL THEN
      exit;
   se_Left.Value := Control.Left;
   se_Top.Value := Control.Top;
   se_Width.Value := Control.Width;
   se_Height.Value := Control.Height;
END;

PROCEDURE TJvgReportEditorForm.AssignEventsToAllComponents;
VAR
   i                          : integer;
BEGIN
   WITH Component.ParentWnd DO
      FOR i := 0 TO ControlCount - 1 DO
         IF (Controls[i] IS TJvgReportItem) THEN
            WITH TJvgReportItem(Controls[i]) DO
            BEGIN
               OnMouseDown := OnMouseDown_;
               OnMouseUp := OnMouseUp_;
               OnMouseMove := OnMouseMove_;
               OnResize := OnResize_;
            END;
END;

FUNCTION CanAlignControl(Control: TControl): boolean;
BEGIN
   Result := (Control IS TJvgReportItem) AND
      (bool(TJvgReportItem(Control).Selected))
      AND (NOT bool(TJvgReportItem(Control).Fixed));
END;

PROCEDURE TJvgReportEditorForm.N4Click(Sender: TObject);
BEGIN
   IF NOT Assigned(AlignForm) THEN
      AlignForm := TAlignForm.Create(NIL);
   IF AlignForm.ShowModal = mrOK THEN
      AlignControlsInWindow(Component.ParentWnd, CanAlignControl,
         AlignForm.Horz, AlignForm.Vert);
END;

PROCEDURE TJvgReportEditorForm.sb_BevelBoldClick(Sender: TObject);
VAR
   i                          : integer;
BEGIN
   WITH Component.ParentWnd DO
      FOR i := 0 TO ControlCount - 1 DO
         IF (Controls[i] IS TJvgReportItem) AND
            bool(TJvgReportItem(Controls[i]).Selected) THEN
            TJvgReportItem(Controls[i]).PenWidth := 1 +
               integer(sb_BevelBold.Down);

END;

PROCEDURE TJvgReportEditorForm.UpdateToolBar(Control: TJvgReportItem);
BEGIN
   WITH Control DO
   BEGIN
      {
        se_Left.Enabled := Assigned(Control);
        se_Top.Enabled := Assigned(Control);
        se_Width.Enabled := Assigned(Control);
        se_Height.Enabled := Assigned(Control);}
      P_Sides.Enabled := Assigned(Control);
      P_Font.Enabled := Assigned(Control);
      P_SBar.Enabled := Assigned(Control);
      IF NOT Assigned(Control) THEN
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
      CASE Alignment OF
         1: sb_AlignL.Down := true;
         2: sb_AlignR.Down := true;
         3: sb_AlignC.Down := true;
         4: sb_AlignW.Down := true;
      END;
      sb_FixMoving.Down := bool(Fixed);
      sb_FontBold.Down := boolean(FStyle AND 1);
      sb_FontItalic.Down := boolean(FStyle AND 2);
      sb_FontUnderline.Down := boolean(FStyle AND 4);
      FE_OLE.Text := OLELinkToFile;
      FE_OLE.Enabled := ContainOLE;
      sb_BevelBold.Down := bool(PenWidth - 1);
      fSkipSizeUpdate := true;
      ShowComponentPos(Control);

      cb_Components.Text := CompName;

   END;
END;

PROCEDURE TJvgReportEditorForm.FormClose(Sender: TObject; VAR Action:
   TCloseAction);
VAR
   msS, msT                   : TMemoryStream;
BEGIN
   IF Assigned(AlignForm) THEN
      AlignForm.Free;
   IF Assigned(ReportParamEditor) THEN
      ReportParamEditor.Free;
   ScrollBox.HorzScrollBar.Position := 0;
   ScrollBox.VertScrollBar.Position := 0;
   Component.Save;
END;

PROCEDURE TJvgReportEditorForm.FE_OLEChange(Sender: TObject);
VAR
   str                        : STRING;
BEGIN
   IF (NOT Assigned(SelectedControl)) OR (NOT FileExists(FE_OLE.Text)) THEN
      exit;
   str := FE_OLE.Text;
   IF ExtractFilePath(Name) = ExtractFilePath(ParamStr(0)) THEN
      str := ExtractFileName(Name);
   IF SelectedControl.OLELinkToFile <> str THEN
      SelectedControl.OLELinkToFile := str;
END;

PROCEDURE TJvgReportEditorForm.SpeedButton1Click(Sender: TObject);
BEGIN
   IF OpenOLEFile.Execute THEN
      FE_OLE.Text := OpenOLEFile.FileName;
END;

PROCEDURE TJvgReportEditorForm.sb_PrintClick(Sender: TObject);
BEGIN
   IF Assigned(Component) AND (Component.ComponentList.Count > 0) THEN
      Component.Print;
   Component.OwnerWnd := self;
   Component.ParentWnd := ScrollBox;
END;

PROCEDURE TJvgReportEditorForm.sb_PreviewClick(Sender: TObject);
VAR
   Form                       : TForm;
   Image                      : TImage;
   bmp                        : TBitmap;
   R                          : TRect;
   i, W, H                    : integer;
BEGIN
   IF NOT Assigned(Component) THEN
      exit;
   Form := TForm.Create(NIL);
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
   TRY
      //    bmp.Canvas.Brush.Color := clWhite;
      //    R := Image.ClientRect; bmp.Canvas.FillRect( R );

      WITH Component DO
         FOR i := 0 TO ComponentList.Count - 1 DO
            WITH TJvgReportItem(ComponentList[i]) DO
            BEGIN
               PaintTo(bmp.Canvas);
               IF ContainOle THEN
                  OLEContainer.PaintTo(bmp.Canvas.Handle, Left, Top);
            END;

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
      Image.Width := W DIV 2;
      Image.Height := H DIV 2;
      Form.ClientWidth := Image.Width;
      Form.ClientHeight := Image.Height;

      bmp.Free;
      bmp := NIL;
      Form.ShowModal;
   FINALLY
      IF Assigned(bmp) THEN
         bmp.Free;
      Form.Free;
   END;
END;

PROCEDURE TJvgReportEditorForm.SpeedButton2Click(Sender: TObject);
BEGIN
   IF NOT Assigned(ReportParamEditor) THEN
      ReportParamEditor := TJvgReportParamEditor.Create(NIL);
   ReportParamEditor.ShowModal;
END;

PROCEDURE TJvgReportEditorForm.cb_ComponentsKeyDown(Sender: TObject; VAR Key:
   Word; Shift: TShiftState);
BEGIN
   IF (Key = VK_RETURN) THEN
   BEGIN
      SelectedControl.CompName := trim(cb_Components.Text);
      RemakeComponentsList;
      cb_Components.Text := SelectedControl.CompName;
   END;
   IF (Key = VK_ESCAPE) THEN
      cb_Components.Text := SelectedControl.CompName;

END;

PROCEDURE TJvgReportEditorForm.CheckBox1Click(Sender: TObject);
BEGIN
   IF Assigned(SelectedControl) THEN
      SelectedControl.Transparent := integer(TCheckBox(Sender).Checked);
END;

END.

