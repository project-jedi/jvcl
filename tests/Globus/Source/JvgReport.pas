{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgReport.PAS, released on 2003-01-15.

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

UNIT JvgReport;

INTERFACE
USES
   Windows,
   Messages,
   Classes,
   Controls,
   Graphics,
   JvgTypes,
   JvgCommClasses,
   JvComponent,
   JvgUtils,
   Forms,
   OleCtnrs,
   ExtCtrls,
   SysUtils,
   Printers;
TYPE

   TJvgReport = CLASS;

   TglRepParamType = (gptUnknown, gptEdit, gptRadio, gptCheck);

   TJvgRepScrollBox = CLASS(TScrollBox)
   PRIVATE
      GridImage: TBitmap;
      PROCEDURE WMEraseBkgnd(VAR Msg: TWMEraseBkgnd); MESSAGE WM_ERASEBKGND;
   PUBLIC
      OnDraw: TNotifyEvent;
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;

   END;

   TJvgReportItem = CLASS(TJvGraphicControl) //TCustomPanel) //TGraphicControl)
   PRIVATE
      FSelected: boolean;
      FBkColor: integer;
      FBvColor: integer;
      FTransparent: integer;
      FAlignment: word;                 //..1-left,2-right,3-center,4-boadwise
      FSideLeft,
         FSideTop,
         FSideRight,
         FSideBottom: word;
      FPenStyle: integer;
      FPenWidth: word;
      FText: STRING;
      PrintText: STRING;
      FCompName: STRING;
      FFName: STRING;
      FFSize,
         FFColor,
         FFStyle: integer;
      FContainOLE: boolean;
      FFixed: word;
      FOLELinkToFile: STRING;
      FOLESizeMode: word;
      fSizing, fRepaintOnlyBorder: boolean;
      R: ARRAY[1..8] OF TRect;
      DownPos: TPoint;
      SizeDirection: integer;
      FExternalCanvas: TCanvas;
      Cursors: ARRAY[1..8] OF TCursor;
      bmp: TBitmap;
      Report: TJvgReport;

      PROCEDURE SetSelected(Value: boolean);
      PROCEDURE SetBkColor(Value: integer);
      PROCEDURE SetBvColor(Value: integer);
      PROCEDURE SetTransparent(Value: integer);
      PROCEDURE SetAlignment(Value: word);
      PROCEDURE SetSideLeft(Value: word);
      PROCEDURE SetSideTop(Value: word);
      PROCEDURE SetSideRight(Value: word);
      PROCEDURE SetSideBottom(Value: word);
      PROCEDURE SetPenStyle(Value: integer);
      PROCEDURE SetPenWidth(Value: word);
      PROCEDURE SetText(Value: STRING);
      PROCEDURE SetFName(Value: STRING);
      PROCEDURE SetFSize(Value: integer);
      PROCEDURE SetFColor(Value: integer);
      PROCEDURE SetFStyle(Value: integer);
      PROCEDURE SetContainOLE(Value: boolean);
      PROCEDURE SetOLELinkToFile(Value: STRING);
      PROCEDURE SetOLESizeMode(Value: word);
      PROCEDURE SetFixed(Value: word);
      FUNCTION IsContainOLE: boolean;

      PROCEDURE CMMouseEnter(VAR Message: TMessage); MESSAGE CM_MOUSEENTER;
      PROCEDURE CMMouseLeave(VAR Message: TMessage); MESSAGE CM_MOUSELEAVE;
      PROCEDURE WMMouseMove(VAR Message: TWMMouse); MESSAGE WM_MOUSEMOVE;
      PROCEDURE WMLMouseDown(VAR Message: TWMMouse); MESSAGE WM_LBUTTONDOWN;
      PROCEDURE WMLMouseUP(VAR Message: TWMMouse); MESSAGE WM_LBUTTONUP;
      PROCEDURE CMFontChanged(VAR Message: TMessage); MESSAGE CM_FONTCHANGED;
      PROCEDURE WMSize(VAR Message: TWMSize); MESSAGE WM_SIZE;

   PUBLIC
      PROCEDURE Paint; OVERRIDE;
      PROCEDURE PaintTo(Canvas: TCanvas);
   PROTECTED
      PROCEDURE SetParent(Value: TWinControl); OVERRIDE;

   PUBLIC
      ResText: STRING;
      OLEContainer: TOLEContainer;
      PROPERTY Selected: boolean READ FSelected WRITE SetSelected DEFAULT false;
      PROPERTY Visible;
      PROPERTY OnClick;
      PROPERTY OnDblClick;
      PROPERTY OnMouseDown;
      PROPERTY OnMouseMove;
      PROPERTY OnMouseUp;
      //    property OnResize;
      PROPERTY ExternalCanvas: TCanvas READ FExternalCanvas WRITE
         FExternalCanvas;
      //    procedure RepaintBorder;
      CONSTRUCTOR Create(AOwner: TComponent);
      DESTRUCTOR Destroy; OVERRIDE;

   PUBLISHED
      PROPERTY BkColor: integer READ FBkColor WRITE SetBkColor DEFAULT clWhite;
      PROPERTY BvColor: integer READ FBvColor WRITE SetBvColor DEFAULT clBlack;
      PROPERTY Transparent: integer READ FTransparent WRITE SetTransparent
         DEFAULT 0;
      PROPERTY Alignment: word READ FAlignment WRITE SetAlignment
         DEFAULT 1;
      PROPERTY SideLeft: word READ FSideLeft WRITE SetSideLeft
         DEFAULT 1;
      PROPERTY SideTop: word READ FSideTop WRITE SetSideTop
         DEFAULT 1;
      PROPERTY SideRight: word READ FSideRight WRITE SetSideRight
         DEFAULT 1;
      PROPERTY SideBottom: word READ FSideBottom WRITE SetSideBottom
         DEFAULT 1;
      PROPERTY PenStyle: integer READ FPenStyle WRITE SetPenStyle
         DEFAULT integer(psSolid);
      PROPERTY PenWidth: word READ FPenWidth WRITE SetPenWidth
         DEFAULT 1;
      PROPERTY Text: STRING READ FText WRITE SetText;
      PROPERTY CompName: STRING READ FCompName WRITE FCompName;
      PROPERTY FName: STRING READ FFName WRITE SetFName;
      PROPERTY FSize: integer READ FFSize WRITE SetFSize;
      PROPERTY FColor: integer READ FFColor WRITE SetFColor;
      PROPERTY FStyle: integer READ FFStyle WRITE SetFStyle;
      PROPERTY ContainOLE: boolean READ FContainOLE WRITE SetContainOLE DEFAULT
         false;
      PROPERTY OLELinkToFile: STRING READ FOLELinkToFile WRITE SetOLELinkToFile
         STORED IsContainOLE;
      PROPERTY OLESizeMode: word READ FOLESizeMode WRITE SetOLESizeMode
         STORED IsContainOLE DEFAULT 2;
      PROPERTY Fixed: word READ FFixed WRITE SetFixed
         DEFAULT 0;
   END;

   TBeforePrintEvent = PROCEDURE(Sender: TJvgReport) OF OBJECT;

   TJvgReport = CLASS(TJvComponent)
   PRIVATE
      PROCEDURE ValidateWnds;
      FUNCTION GetReportText: TStringList;
      PROCEDURE SetReportText(Value: TStringList);
   PUBLIC
      OwnerWnd,
         ParentWnd: TWinControl;
      ParamNames: TStringList;
      ParamValues: TStringList;
      ParamMasks: TStringList;
      ParamTypes: TList;
      FReportList: TStringList;
      ComponentList: TList;
      FBeforePrint: TBeforePrintEvent;
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;
      PROCEDURE Save;
      PROCEDURE LoadFromFile(FileName: STRING);
      PROCEDURE SaveToFile(FileName: STRING);
      PROCEDURE PaintTo(Canvas: TCanvas);
      PROCEDURE PreviewTo(Window: TWinControl);
      PROCEDURE Print;
      PROCEDURE CreateReport(ParentWnd: TWinControl; fNeedClearOwner: boolean);
      FUNCTION SetParam(CONST sParamName, sParamValue: STRING): boolean;
      FUNCTION GetParam(CONST sParamName: STRING; VAR sParamValue: STRING):
         boolean;
      FUNCTION AddComponent: TJvgReportItem;
      PROCEDURE AnalyzeParams(Item: TJvgReportItem; DefName: STRING);
   PRIVATE
      PROCEDURE SetUnicalName(laBevel: TJvgReportItem);
   PROTECTED
      PROCEDURE Loaded; OVERRIDE;
      PROCEDURE ClearReport;
   PUBLISHED
      PROPERTY Report: TStringList READ FReportList;
      PROPERTY ReportText: TStringList READ GetReportText WRITE SetReportText;
      PROPERTY BeforePrint: TBeforePrintEvent READ FBeforePrint WRITE
         FBeforePrint;
   END;

PROCEDURE Register;
IMPLEMENTATION

CONST
   S                          = 2;
   DS                         = 2 * S + 1;
   {~~~~~~~~~~~~~~~~~~~~~~~~~}

PROCEDURE Register;
BEGIN
END;
{~~~~~~~~~~~~~~~~~~~~~~~~~}
//________________________________________________________ Methods _

CONSTRUCTOR TJvgRepScrollBox.Create(AOwner: TComponent);
BEGIN
   INHERITED Create(AOwner);
   GridImage := TBitmap.Create;
   GridImage.Width := 8;
   GridImage.Height := 8;
   GridImage.Canvas.Brush.Color := clWhite; //clWindow;
   GridImage.Canvas.FillRect(Rect(0, 0, 8, 8));
   GridImage.Canvas.Pixels[7, 7] := 0;
END;

DESTRUCTOR TJvgRepScrollBox.Destroy;
BEGIN
   GridImage.Free;
   INHERITED;
END;

PROCEDURE TJvgRepScrollBox.WMEraseBkgnd(VAR Msg: TWMEraseBkgnd);
VAR
   DC                         : HDC;
   Canvas                     : TCanvas;
BEGIN
   WITH TCanvas.Create DO
   BEGIN
      Handle := Msg.DC;
      //    Pen.Color := clWindow;
      //    Brush.Color := clWindow;
      //    Brush.Style := bsCross;
      Brush.Bitmap := GridImage;
      FillRect(ClientRect);
      Handle := 0;
      Free;
   END;
   Msg.Result := 1;
   IF Assigned(OnDraw) THEN
      OnDraw(self);
END;

CONSTRUCTOR TJvgReportItem.Create(AOwner: TComponent);
VAR
   Msg                        : TMessage;
BEGIN
   INHERITED Create(AOwner);
   //..defaults
   Width := 50;
   Height := 50;
   Color := clWhite;
   FBkColor := clWhite;
   FBvColor := clBlack;
   FAlignment := 1;
   FSideLeft := 1;
   FSideTop := 1;
   FSideRight := 1;
   FSideBottom := 1;
   FPenStyle := integer(psSolid);
   FPenWidth := 1;
   FOLESizeMode := 2;
   Cursors[1] := crSizeNWSE;
   Cursors[2] := crSizeNS;
   Cursors[3] := crSizeNESW;

   Cursors[4] := crSizeNESW;
   Cursors[5] := crSizeNS;
   Cursors[6] := crSizeNWSE;

   Cursors[7] := crSizeWE;
   Cursors[8] := crSizeWE;
   ParentFont := false;
   {$IFDEF GL_RUS}
   Font.CharSet := RUSSIAN_CHARSET;
   {$ENDIF}
   CMFontChanged(Msg);
END;

DESTRUCTOR TJvgReportItem.Destroy;
BEGIN
   IF Assigned(bmp) THEN
      bmp.Free;
   IF Assigned(OLEContainer) THEN
   BEGIN
      OLEContainer.DestroyObject;
      IF NOT (csDestroying IN ComponentState) THEN
      BEGIN
         OLEContainer.Free;
         OLEContainer := NIL;
      END;
   END;
   INHERITED;
END;

{procedure TJvgReportItem.RepaintBorder;
var R: TRect;
begin
  R := ClientRect;
  OffsetRect( R, Left, Top );
  InvalidateRect( Parent.Handle, @R, false );
  InflateRect( R, -DS, -DS );
//  ValidateRect( Parent.Handle, @R );
  fRepaintOnlyBorder := true;
  Paint;
  //fRepaintOnlyBorder := false;
end;
}

PROCEDURE TJvgReportItem.Paint;
BEGIN
   PaintTo(Canvas);
END;

PROCEDURE TJvgReportItem.PaintTo(Canvas: TCanvas);
CONST
   Alignments                    : ARRAY[1..4] OF TglAlignment = (ftaLeftJustify,
      ftaRightJustify, ftaCenter, ftaBroadwise);
   //  SysAlignments: array[TglAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER, 0);
VAR
   i, L, T                    : integer;
   sPrintText                 : STRING;
   R_, Client_Rect, RCalc     : TRect;
BEGIN
   FFColor := 0;
   WITH Canvas DO
   BEGIN

      IF Canvas = Self.Canvas THEN
         Client_Rect := Rect(0, 0, Width, height)
      ELSE
      BEGIN
         Client_Rect := Bounds(Left, Top, Width, height);
         Canvas.Font := Self.Canvas.Font;
         Canvas.Font.Color := 0;
      END;
      R_ := Client_Rect;
      L := Client_Rect.Left;
      T := Client_Rect.Top;
      InflateRect(R_, -DS, -S);
      RCalc := R_;

      IF Transparent = 0 THEN
      BEGIN
         Brush.Color := BkColor;
         FillRect(Client_Rect);
      END;

      IF Canvas = Self.Canvas THEN
      BEGIN
         Pen.Style := psDot;
         Pen.Width := 1;
         Pen.Color := clSilver;
         Brush.Style := bsClear;
         Rectangle(L, T, L + Width, T + Height);
         sPrintText := Text;
      END
      ELSE
         sPrintText := PrintText;
      IF sPrintText = '' THEN
         sPrintText := Text;

      Pen.Style := TPenStyle(PenStyle);
      Pen.Width := PenWidth;
      Pen.Color := BvColor;
      IF bool(SideLeft) THEN
      BEGIN
         MoveTo(L + PenWidth DIV 2, T + Height - 1);
         LineTo(L + PenWidth DIV 2, T);
      END;
      IF bool(SideTop) THEN
      BEGIN
         MoveTo(L + PenWidth DIV 2, T + PenWidth DIV 2);
         LineTo(L + Width - PenWidth, T + PenWidth DIV 2);
      END;
      IF bool(SideRight) THEN
      BEGIN
         MoveTo(L + Width - 1, T);
         LineTo(L + Width - 1, T + Height - 1);
      END;
      IF bool(SideBottom) THEN
      BEGIN
         MoveTo(L + Width - 1, T + Height - 1);
         LineTo(L, T + Height - 1);
      END;

      IF NOT ContainOLE THEN
      BEGIN
         SetBkMode(Canvas.Handle, TRANSPARENT);
         SetTextColor(Canvas.Handle, FColor);
         DrawText(Canvas.Handle, PChar(sPrintText), Length(sPrintText), RCalc,
            DT_CALCRECT OR DT_WORDBREAK);
         R_.Top := R_.Top + max(0, (R_.Bottom - R_.Top - (RCalc.Bottom -
            RCalc.Top)) DIV 2);
         DrawTextExtAligned(Canvas, sPrintText, R_, Alignments[Alignment],
            true);
      END
      ELSE IF (OLELinkToFile <> '') AND (ExtractFileExt(OLELinkToFile) = '.bmp')
         THEN
      BEGIN
         IF Assigned(OLEContainer) THEN
            OLEContainer.Visible := false;
         IF bmp = NIL THEN
         BEGIN
            bmp := TBitmap.Create;
            bmp.LoadFromFile(OLELinkToFile);
         END;
         BitBlt(Canvas.Handle, L, T, bmp.width, bmp.height, bmp.Canvas.handle,
            0, 0, SRCCOPY);
      END;

      IF Selected THEN
      BEGIN
         Pen.Style := psSolid;
         Pen.Width := 1;
         Pen.Color := 0;
         Brush.Style := bsSolid;
         IF bool(Fixed) THEN
            Brush.Color := clBtnFace
         ELSE
            Brush.Color := clWhite;
         R[1] := Rect(0, 0, DS, DS);    //...top-left
         R[2] := Rect(Width DIV 2 - S, 0, Width DIV 2 + S + 1, DS);  //...top-center
         R[3] := Rect(Width - DS, 0, Width, DS); //...top-right

         R[4] := Rect(0, Height - DS, DS, Height); //...bottom-left
         R[5] := Rect(Width DIV 2 - S, Height - DS, Width DIV 2 + S + 1, Height);  //...bottom-center
         R[6] := Rect(Width - DS, Height - DS, Width, Height); //...bottom-right

         R[7] := Rect(0, Height DIV 2 - S, DS, Height DIV 2 + S + 1);  //...left-center
         R[8] := Rect(Width - DS, Height DIV 2 - S, Width, Height DIV 2 + S + 1);  //...right-center

         FOR i := 1 TO 8 DO
            Rectangle(R[i].Left, R[i].Top, R[i].Right, R[i].Bottom);
      END;
   END;
   IF Assigned(OLEContainer) THEN
      OLEContainer.SetBounds(Left + DS, Top + DS, Width - 2 * DS, Height - 2 *
         DS);
END;

PROCEDURE TJvgReportItem.SetParent(Value: TWinControl);
BEGIN
   INHERITED;
   IF Assigned(OLEContainer) AND Assigned(Value) THEN
      OLEContainer.Parent := Value;
END;

PROCEDURE TJvgReportItem.CMMouseEnter(VAR Message: TMessage);
BEGIN
   //Cursor := crCross;
 //  SetCursor( Screen.Cursors[crCross] );
END;

PROCEDURE TJvgReportItem.CMMouseLeave(VAR Message: TMessage);
BEGIN
   Cursor := crDefault;
   //  SetCursor( Screen.Cursors[crDefault] );
END;

PROCEDURE TJvgReportItem.WMMouseMove(VAR Message: TWMMouse);
VAR
   i, dX, dY, nLeft, nTop, nWidth, nHeight: integer;
   pt                         : TPoint;
BEGIN
   INHERITED;
   IF NOT bool(Fixed) THEN
      WITH Message DO
      BEGIN
         pt.x := Pos.x;
         pt.y := Pos.y;
         IF fSizing THEN
         BEGIN
            dX := Pos.x - DownPos.x;
            dY := Pos.y - DownPos.y;
            inc(pos.x, 4);
            inc(pos.y, 4);
            nLeft := Left;
            nTop := Top;
            nWidth := Width;
            nHeight := Height;
            CASE SizeDirection OF
               1:
                  BEGIN
                     nLeft := Left + dX;
                     nWidth := Width - dX;
                     nTop := Top + dY;
                     nHeight := Height - dY;
                  END;
               2:
                  BEGIN
                     nTop := Top + dY;
                     nHeight := Height - dY;
                  END;
               3:
                  BEGIN
                     nWidth := Pos.x;
                     nTop := Top + dY;
                     nHeight := Height - dY;
                  END;

               4:
                  BEGIN
                     nLeft := Left + dX;
                     nWidth := Width - dX;
                     nHeight := Pos.y;
                  END;

               5:
                  BEGIN
                     nHeight := Pos.y;
                  END;

               6:
                  BEGIN
                     nWidth := Pos.x;
                     nHeight := Pos.y;
                  END;
               7:
                  BEGIN
                     nLeft := Left + dX;
                     nWidth := Width - dX;
                  END;
               8:
                  BEGIN
                     nWidth := Pos.x;
                  END;
            END;
            Left := min(nLeft, nLeft + nWidth);
            Top := min(nTop, nTop + nHeight);
            Width := abs(nWidth);
            Height := abs(nHeight);
            IF nWidth < 0 THEN
            BEGIN
               CASE SizeDirection OF
                  1: SizeDirection := 3;
                  3: SizeDirection := 1;
                  4: SizeDirection := 6;
                  6: SizeDirection := 4;
                  8: SizeDirection := 7;
                  7: SizeDirection := 8;
               END;
               DownPos.x := Pos.x;
            END;
            IF nHeight < 0 THEN
            BEGIN
               CASE SizeDirection OF
                  1: SizeDirection := 4;
                  2: SizeDirection := 5;
                  3: SizeDirection := 6;
                  4: SizeDirection := 1;
                  5: SizeDirection := 2;
                  6: SizeDirection := 3;
               END;
               DownPos.y := Pos.y;
            END;
         END
         ELSE
            FOR i := 1 TO 8 DO
               IF PtInRect(R[i], pt) THEN
               BEGIN
                  Cursor := Cursors[i];
                  SizeDirection := i;
                  exit;
               END;
      END;
   Cursor := crDefault;
   //  SetCursor( Screen.Cursors[crDefault] );
END;

PROCEDURE TJvgReportItem.WMLMouseDown(VAR Message: TWMMouse);
BEGIN
   DownPos.x := Message.Pos.x;
   DownPos.y := Message.Pos.y;
   //DownPos := ClientToScreen(DownPos);
   fSizing := Cursor <> crDefault;
   INHERITED;
END;

{procedure TJvgReportItem.WMRMouseDown(var Message: TWMMouse);
begin
  DownPos.x := Message.Pos.x; DownPos.y := Message.Pos.y;
  if Assigned(PopupMenu)
  inherited;
end;}

PROCEDURE TJvgReportItem.WMLMouseUp(VAR Message: TWMMouse);
BEGIN
   fSizing := false;
   INHERITED;
END;

PROCEDURE TJvgReportItem.CMFontChanged(VAR Message: TMessage);
BEGIN
   INHERITED;
   FName := Font.Name;
   FFSize := Font.Size;
   FFColor := Font.Color;
   FFStyle := 0;
   IF fsBold IN Font.Style THEN
      FFStyle := FFStyle OR 1;
   IF fsItalic IN Font.Style THEN
      FFStyle := FFStyle OR (1 SHL 1);
   IF fsUnderline IN Font.Style THEN
      FFStyle := FFStyle OR (1 SHL 2);
   Invalidate;
END;

PROCEDURE TJvgReportItem.WMSize(VAR Message: TWMSize);
BEGIN
   INHERITED;
   // if Assigned(OnResize) then OnResize(self);
END;

PROCEDURE TJvgReportItem.SetSelected(Value: boolean);
BEGIN
   FSelected := Value;
   Repaint;
END;

PROCEDURE TJvgReportItem.SetBkColor(Value: integer);
BEGIN
   FBkColor := Value;
   Color := BkColor;
   Repaint;
END;

PROCEDURE TJvgReportItem.SetBvColor(Value: integer);
BEGIN
   FBvColor := Value;
   Repaint;
END;

PROCEDURE TJvgReportItem.SetTransparent(Value: integer);
BEGIN
   FTransparent := Value;
   Repaint;
END;

PROCEDURE TJvgReportItem.SetAlignment(Value: word);
BEGIN
   FAlignment := Value;
   Invalidate;
END;

PROCEDURE TJvgReportItem.SetSideLeft(Value: word);
BEGIN
   FSideLeft := Value;
   Invalidate;
END;

PROCEDURE TJvgReportItem.SetSideTop(Value: word);
BEGIN
   FSideTop := Value;
   Invalidate;
END;

PROCEDURE TJvgReportItem.SetSideRight(Value: word);
BEGIN
   FSideRight := Value;
   Invalidate;
END;

PROCEDURE TJvgReportItem.SetSideBottom(Value: word);
BEGIN
   FSideBottom := Value;
   Invalidate;
END;

PROCEDURE TJvgReportItem.SetPenStyle(Value: integer);
BEGIN
   FPenStyle := Value;
   Invalidate;
END;

PROCEDURE TJvgReportItem.SetPenWidth(Value: word);
BEGIN
   FPenWidth := Value;
   Invalidate;
END;

PROCEDURE TJvgReportItem.SetText(Value: STRING);
BEGIN
   IF FText = Value THEN
      exit;
   FText := Value;
   Invalidate;
END;

PROCEDURE TJvgReportItem.SetFName(Value: STRING);
BEGIN
   FFName := Value;
   Canvas.Font.Name := Value;
   Invalidate;
END;

PROCEDURE TJvgReportItem.SetFSize(Value: integer);
BEGIN
   FFSize := Value;
   Canvas.Font.Size := Value;
   Invalidate;
END;

PROCEDURE TJvgReportItem.SetFColor(Value: integer);
BEGIN
   FFColor := Value;
   Canvas.Font.Color := Value;
   Invalidate;
END;

PROCEDURE TJvgReportItem.SetFStyle(Value: integer);
BEGIN
   FFStyle := Value;
   WITH Canvas.Font DO
   BEGIN
      IF bool(Value AND 1) THEN
         Style := Style + [fsBold]
      ELSE
         Style := Style - [fsBold];
      IF bool(Value AND (1 SHL 1)) THEN
         Style := Style + [fsItalic]
      ELSE
         Style := Style - [fsItalic];
      IF bool(Value AND (1 SHL 2)) THEN
         Style := Style + [fsUnderline]
      ELSE
         Style := Style - [fsUnderline];
   END;
   Invalidate;
END;

PROCEDURE TJvgReportItem.SetContainOLE(Value: boolean);
BEGIN
   FContainOLE := Value;
   IF FContainOLE AND (NOT Assigned(OLEContainer)) THEN
   BEGIN
      IF NOT Assigned(Parent) THEN
         exit;
      OLEContainer := TOLEContainer.Create(parent.parent);  //{$IFDEF COMPILER3_UP} Parent {$ELSE} Owner {$ENDIF} );
      OLEContainer.AutoVerbMenu := false;
      OLEContainer.BorderStyle := bsNone;
      OLEContainer.Color := clWhite;
      OLEContainer.SizeMode := smScale;
      OLEContainer.Parent := Parent;
      IF (OLEContainer.State = osEmpty) AND (OLELinkToFile <> '') THEN
         SetOLELinkToFile(OLELinkToFile);
   END;
END;

PROCEDURE TJvgReportItem.SetOLELinkToFile(Value: STRING);
BEGIN
   FOLELinkToFile := Value;
   IF NOT Assigned(OLEContainer) THEN
      exit;
   OLEContainer.CreateLinkToFile(Value, False);
   //OLEContainer.LoadFromFile( Value );
END;

PROCEDURE TJvgReportItem.SetFixed(Value: word);
BEGIN
   FFixed := Value;
   Repaint;
END;

PROCEDURE TJvgReportItem.SetOLESizeMode(Value: word);
BEGIN
   IF FOLESizeMode = Value THEN
      exit;
   FOLESizeMode := Value;
   IF Assigned(OLEContainer) THEN
      OLEContainer.SizeMode := TSizeMode(Value);
END;

FUNCTION TJvgReportItem.IsContainOLE: boolean;
BEGIN
   Result := FContainOLE;
END;

//===========================================================================

CONSTRUCTOR TJvgReport.Create(AOwner: TComponent);
BEGIN
   INHERITED;
   ParamNames := TStringList.Create;
   ParamValues := TStringList.Create;
   ParamMasks := TStringList.Create;
   FReportList := TStringList.Create;
   ParamTypes := TList.Create;
   ComponentList := TList.Create;
END;

DESTRUCTOR TJvgReport.Destroy;
BEGIN
   FReportList.Free;
   ParamNames.Free;
   ParamValues.Free;
   ParamMasks.Free;
   ParamTypes.Free;
   ClearReport;
   ComponentList.Free;
   INHERITED;
END;

PROCEDURE TJvgReport.Loaded;
BEGIN
   INHERITED;
   CreateReport(NIL, false);
END;

PROCEDURE TJvgReport.Save;
VAR
   msS, msT                   : TMemoryStream;
BEGIN
   ValidateWnds;
   msS := TMemoryStream.Create;
   msT := TMemoryStream.Create;
   TRY
      msS.WriteComponent(ParentWnd);
      msS.Position := 0;
      ObjectBinaryToText(msS, msT);
      msT.Position := 0;
      FReportList.LoadFromStream(msT);
   FINALLY
      msS.Free;
      msT.Free;
   END;
END;

PROCEDURE TJvgReport.SaveToFile(FileName: STRING);
VAR
   fs, fs2                    : TFileStream;
   W                          : TWriter;
   i                          : integer;
BEGIN
   ValidateWnds;
   fs := TFileStream.Create(FileName, fmCreate OR fmOpenWrite);
   TRY
      fs.WriteComponent(ParentWnd);
   FINALLY
      fs.Free;
   END;
END;

PROCEDURE TJvgReport.LoadFromFile(FileName: STRING);
VAR
   fs                         : TFileStream;
   ms                         : TMemoryStream;
BEGIN
   fs := TFileStream.Create(FileName, fmOpenRead);
   ms := TMemoryStream.Create;
   TRY
      ObjectBinaryToText(fs, ms);
      ms.Position := 0;
      FReportList.LoadFromStream(ms);
   FINALLY
      fs.Free;
      ms.Free;
   END;
END;

{procedure TJvgReport.Edit;
begin
  CreateReport(true);
end;}

PROCEDURE TJvgReport.PaintTo(Canvas: TCanvas);
VAR
   i                          : integer;
BEGIN
   OwnerWnd := NIL;
   ParentWnd := NIL;
   //  ParamNames.Clear;
   //  ParamMasks.Clear;
   //  ParamValues.Clear;
   //  ParamTypes.Clear;
   ComponentList.Clear;
   CreateReport(ParentWnd, false);
   FOR i := 0 TO ComponentList.Count - 1 DO
      TJvgReportItem(ComponentList[i]).PaintTo(Canvas);
END;

PROCEDURE TJvgReport.PreviewTo(Window: TWinControl);
BEGIN
   OwnerWnd := Window;
   ParentWnd := OwnerWnd;
   ParamNames.Clear;
   ParamMasks.Clear;
   ParamValues.Clear;
   ParamTypes.Clear;
   ComponentList.Clear;
   CreateReport(ParentWnd, false);
   //  ProcessParams;
END;

PROCEDURE TJvgReport.Print;
VAR
   i                          : integer;
   ScreenDC                   : HDC;
   HS, WS, HP, WP             : integer;
BEGIN
   IF Assigned(BeforePrint) THEN
      BeforePrint(self);
   OwnerWnd := TForm.Create(NIL);
   TForm(OwnerWnd).WindowState := wsMaximized;
   ParentWnd := OwnerWnd;
   //OwnerWnd.Show;
   TRY
      CreateReport(ParentWnd, true);
      IF ComponentList.Count = 0 THEN
         exit;

      Printer.BeginDoc;
      ScreenDC := GetDC(0);

      HS := SantimsToPixels(ScreenDC, 21, true);
      WS := SantimsToPixels(ScreenDC, 21, false);
      HP := SantimsToPixels(Printer.Canvas.Handle, 21, true);
      WP := SantimsToPixels(Printer.Canvas.Handle, 21, false);

      ReleaseDC(0, ScreenDC);

      FOR i := 0 TO ComponentList.Count - 1 DO
      BEGIN
         TJvgReportItem(ComponentList[i]).Left :=
            MulDiv(TJvgReportItem(ComponentList[i]).Left, WP, WS);
         TJvgReportItem(ComponentList[i]).Top :=
            MulDiv(TJvgReportItem(ComponentList[i]).Top, HP, HS);
         TJvgReportItem(ComponentList[i]).Width :=
            MulDiv(TJvgReportItem(ComponentList[i]).Width, WP, WS);
         TJvgReportItem(ComponentList[i]).Height :=
            MulDiv(TJvgReportItem(ComponentList[i]).Height, HP, HS);
         TJvgReportItem(ComponentList[i]).PenWidth :=
            MulDiv(TJvgReportItem(ComponentList[i]).PenWidth, HP, HS);
      END;

      FOR i := 0 TO ComponentList.Count - 1 DO
         WITH TJvgReportItem(ComponentList[i]) DO
         BEGIN
            PaintTo(Printer.Canvas);
            IF ContainOle THEN
               OLEContainer.PaintTo(Printer.Canvas.Handle, Left, Top);
         END;
      Printer.EndDoc;

      REPEAT Application.ProcessMessages;
      UNTIL NOT TForm(OwnerWnd).Active;
   FINALLY
      OwnerWnd.Free;
   END;
END;

PROCEDURE TJvgReport.ClearReport;
VAR
   i                          : integer;
BEGIN
   FOR i := 0 TO ComponentList.Count - 1 DO
      TJvgReportItem(ComponentList[i]).Free;
   ComponentList.Count := 0;
END;

PROCEDURE TJvgReport.CreateReport(ParentWnd: TWinControl; fNeedClearOwner:
   boolean);
VAR
   fs, fs2                    : TFileStream;
   ms                         : TMemoryStream;
   p                          : TParser;
   c                          : char;
   i                          : integer;
   Compon                     : TComponent;
   sName, sClassName          : STRING;

   PROCEDURE N2T;
   BEGIN
      p.NextToken;
      p.NextToken;
   END;

   PROCEDURE Create_Object(sClassName, sName: STRING);
   VAR
      B                       : TJvgReportItem;
   BEGIN
      B := NIL;
      IF sClassName = 'TJvgReportItem' THEN  //...process only TJvgReportItem class
      BEGIN
         B := TJvgReportItem.Create(OwnerWnd);
         B.Report := self;
      END;
      IF B = NIL THEN
         exit;
      ComponentList.Add(B);
      c := p.NextToken;
      WHILE NOT p.TokenSymbolIs('end') DO
         WITH p DO
         BEGIN
            CASE c OF
               '+':
                  BEGIN
                     p.NextToken;
                     b.Text := b.Text + TokenString;
                  END;
               toSymbol:
                  BEGIN
                     IF TokenString = 'Left' THEN
                     BEGIN
                        N2T;
                        b.Left := TokenInt;
                     END;
                     IF TokenString = 'Top' THEN
                     BEGIN
                        N2T;
                        b.Top := TokenInt;
                     END;
                     IF TokenString = 'Width' THEN
                     BEGIN
                        N2T;
                        b.Width := TokenInt;
                     END;
                     IF TokenString = 'Height' THEN
                     BEGIN
                        N2T;
                        b.Height := TokenInt;
                     END;
                     IF TokenString = 'Text' THEN
                     BEGIN
                        N2T;
                        b.Text := TokenString;
                     END;
                     IF TokenString = 'BkColor' THEN
                     BEGIN
                        N2T;
                        b.BkColor := TokenInt;
                     END;
                     IF TokenString = 'BvColor' THEN
                     BEGIN
                        N2T;
                        b.BvColor := TokenInt;
                     END;
                     IF TokenString = 'Transparent' THEN
                     BEGIN
                        N2T;
                        b.Transparent := TokenInt;
                     END;
                     IF TokenString = 'Alignment' THEN
                     BEGIN
                        N2T;
                        b.Alignment := TokenInt;
                     END;
                     IF TokenString = 'SideLeft' THEN
                     BEGIN
                        N2T;
                        b.SideLeft := TokenInt;
                     END;
                     IF TokenString = 'SideTop' THEN
                     BEGIN
                        N2T;
                        b.SideTop := TokenInt;
                     END;
                     IF TokenString = 'SideRight' THEN
                     BEGIN
                        N2T;
                        b.SideRight := TokenInt;
                     END;
                     IF TokenString = 'SideBottom' THEN
                     BEGIN
                        N2T;
                        b.SideBottom := TokenInt;
                     END;
                     IF TokenString = 'PenStyle' THEN
                     BEGIN
                        N2T;
                        b.PenStyle := TokenInt;
                     END;
                     IF TokenString = 'PenWidth' THEN
                     BEGIN
                        N2T;
                        b.PenWidth := TokenInt;
                     END;
                     IF TokenString = 'CompName' THEN
                     BEGIN
                        N2T;
                        b.CompName := TokenString;
                     END;
                     IF TokenString = 'FName' THEN
                     BEGIN
                        N2T;
                        b.FName := TokenString;
                     END;
                     IF TokenString = 'FSize' THEN
                     BEGIN
                        N2T;
                        b.FSize := TokenInt;
                     END;
                     IF TokenString = 'FColor' THEN
                     BEGIN
                        N2T;
                        b.FColor := TokenInt;
                     END;
                     IF TokenString = 'FStyle' THEN
                     BEGIN
                        N2T;
                        b.FStyle := TokenInt;
                     END;
                     IF TokenString = 'OLELinkToFile' THEN
                     BEGIN
                        N2T;
                        b.OLELinkToFile := TokenString;
                     END;
                     IF TokenString = 'OLESizeMode' THEN
                     BEGIN
                        N2T;
                        b.OLESizeMode := TokenInt;
                     END;
                     IF TokenString = 'Fixed' THEN
                     BEGIN
                        N2T;
                        b.Fixed := TokenInt;
                     END;
                  END;
            END;
            c := NextToken;
         END;

      B.Parent := ParentWnd;
      TRY
         B.ContainOLE := B.OLELinkToFile <> '';
      EXCEPT
         Application.MessageBox('OLE: Linked object not found.', 'Error',
            MB_APPLMODAL OR MB_OK OR MB_ICONSTOP);
      END;
      B.Name := sName;
      IF B.CompName = '' THEN
         SetUnicalName(B);
      AnalyzeParams(B, B.CompName);
   END;

   PROCEDURE ClearOwner;
   VAR
      i                       : integer;
   BEGIN
      //    ParamNames.Clear;
      //    ParamMasks.Clear;
      //    ParamValues.Clear;
      //    ParamTypes.Clear;
      ComponentList.Clear;
      IF Assigned(ParentWnd) THEN
      BEGIN
         WITH ParentWnd DO
            FOR i := ControlCount - 1 DOWNTO 0 DO
               IF Controls[i] IS TJvgReportItem THEN
                  RemoveControl(Controls[i]);
         WITH OwnerWnd DO
            FOR i := ComponentCount - 1 DOWNTO 0 DO
            BEGIN
               IF Components[i] IS TJvgReportItem THEN
               BEGIN
                  Compon := Components[i];
                  RemoveComponent(Compon);
                  Compon.Free;
               END;
            END;
      END;
   END;

BEGIN
   ValidateWnds;
   IF fNeedClearOwner THEN
      ClearOwner
   ELSE
      ClearReport;
   ms := TMemoryStream.Create;
   FReportList.SaveToStream(ms);
   ms.Position := 0;
   p := TParser.Create(ms);
   c := p.Token;
   WITH p DO
      REPEAT
         IF TokenSymbolIs('object') THEN //...only noname objects!
         BEGIN
            NextToken;
            sClassName := TokenString;
            TRY
               Create_Object(sClassName, sName);
            EXCEPT
               Application.MessageBox('Error reading component.', 'Error',
                  MB_APPLMODAL OR MB_OK OR MB_ICONSTOP);
            END;
         END;
         c := NextToken;
      UNTIL c = toEOF;

   p.Free;
   ms.Free;
END;

FUNCTION TJvgReport.AddComponent: TJvgReportItem;
BEGIN
   //AnalyzeParams( ReportComponent );
   ValidateWnds;
   Result := TJvgReportItem.Create(OwnerWnd);
   Result.Report := self;
   SetUnicalName(Result);
   Result.Parent := ParentWnd;
   ComponentList.Add(Result);
END;

PROCEDURE TJvgReport.SetUnicalName(laBevel: TJvgReportItem);
VAR
   i                          : integer;

   FUNCTION ComponentExists(No: integer): boolean;
   VAR
      i                       : integer;
   BEGIN
      Result := false;
      FOR i := 0 TO OwnerWnd.ComponentCount - 1 DO
         IF OwnerWnd.Components[i] IS TJvgReportItem THEN
            IF TJvgReportItem(OwnerWnd.Components[i]).CompName = 'Component' +
               IntToStr(No) THEN
            BEGIN
               Result := true;
               break;
            END;
   END;
BEGIN
   i := 0;
   REPEAT
      inc(i);
   UNTIL NOT ComponentExists(i);
   laBevel.CompName := 'Component' + IntToStr(i);
END;

PROCEDURE TJvgReport.AnalyzeParams(Item: TJvgReportItem; DefName: STRING);
VAR
   ParamIndex, LastPos        : integer;
   SList                      : TStringList;
   ParamType                  : TglRepParamType;
   ParamText, ParamName, ParamMask, ParamValue: STRING;

   FUNCTION ExtractParam(Item: TJvgReportItem; VAR SrchPos: integer; VAR
      ParamName: STRING; VAR ParamType: TglRepParamType): boolean;
   VAR
      i, j, p                 : integer;
      f                       : boolean;
      Text                    : STRING;
   BEGIN
      Result := false;
      Text := Item.Text;
      IF Length(Text) = 0 THEN
         exit;
      f := false;
      FOR i := SrchPos TO Length(Text) - 1 DO
         IF Text[i] = '#' THEN
         BEGIN
            f := true;
            break;
         END;

      IF NOT f THEN
         exit;
      IF Text[i - 1] = '{' THEN
         ParamType := gptEdit
      ELSE IF Text[i - 1] = '<' THEN
         ParamType := gptRadio
      ELSE IF Text[i - 1] = '[' THEN
         ParamType := gptCheck
      ELSE
         ParamType := gptUnknown;

      IF NOT f OR (ParamType = gptUnknown) THEN
         exit;
      SrchPos := i + 1;
      f := false;
      FOR i := SrchPos TO Length(Text) DO
         IF (Text[i] = '}') OR (Text[i] = ']') OR (Text[i] = '>') THEN
         BEGIN
            f := true;
            break;
         END;
      IF NOT f THEN
         exit;
      ParamName := copy(Text, SrchPos, i - SrchPos);

      j := ParamNames.IndexOf(ParamName);
      IF j <> -1 THEN
         Item.PrintText := copy(Text, 0, SrchPos - 3) + ParamValues[j] +
            copy(Text, i + 1, 255);

      Result := true;
   END;
BEGIN

   LastPos := 0;
   SList := TStringList.Create;
   REPEAT

      IF ExtractParam(Item, LastPos, ParamText, ParamType) THEN
      BEGIN
         ParamMask := '';
         ParamValue := '';
         ParamTypes.Add(Pointer(ParamType));
         IF ParamType = gptEdit THEN
         BEGIN
            IF ParamText = '' THEN
               ParamText := DefName;
            SList.CommaText := ParamText;
            IF SList.Count = 0 THEN
               continue;
            ParamName := SList[0];
            IF SList.Count > 1 THEN
               ParamMask := SList[1];
            IF SList.Count > 2 THEN
               ParamValue := SList[2];
         END
         ELSE
            ParamName := ParamText;
         IF ParamNames.IndexOf(ParamName) <> -1 THEN
            continue;                   //...already exists
         ParamNames.Add(ParamName);
         ParamMasks.Add(ParamMask);
         ParamValues.Add(ParamValue);
         // else ParamValues[ParamIndex] := sParamValue;
      END
      ELSE
         break;
   UNTIL false;
   SList.Free;
END;

FUNCTION TJvgReport.SetParam(CONST sParamName, sParamValue: STRING): boolean;
VAR
   i                          : integer;
BEGIN
   Result := false;
   i := ParamNames.IndexOf(sParamName);
   IF i <> -1 THEN
   BEGIN
      Result := true;
      ParamValues[i] := sParamValue;
   END;
END;

FUNCTION TJvgReport.GetParam(CONST sParamName: STRING; VAR sParamValue: STRING):
   boolean;
VAR
   i, ParamIndex              : integer;
BEGIN
   ParamIndex := ParamNames.IndexOf(sParamName);
   IF ParamIndex = -1 THEN
      Result := false
   ELSE
   BEGIN
      Result := true;
      sParamValue := ParamValues[ParamIndex];
   END;
END;

PROCEDURE TJvgReport.ValidateWnds;
BEGIN
   {$IFDEF COMPILER3_UP}OwnerWnd := ParentWnd;
   {$ENDIF}
   //  if (OwnerWnd=nil)or(ParentWnd=nil) then raise Exception.Create('TJvgReport: Unassigned Owner or Parent window.');
END;

FUNCTION TJvgReport.GetReportText: TStringList;
BEGIN
   Result := FReportList;
END;

PROCEDURE TJvgReport.SetReportText(Value: TStringList);
BEGIN
   FReportList.Assign(Value);
END;

END.

