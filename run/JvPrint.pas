{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPrint.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is S�bastien Buysse [sbuysse att buypin dott com]
Portions created by S�bastien Buysse are Copyright (C) 2001 S�bastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvPrint;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, SysUtils, Classes, Graphics, Printers,
  JvTypes, JvComponentBase;

type
  TJvPrintMeasureItemEvent = procedure(Sender: TObject; ACanvas: TCanvas;
    AIndex: Integer; const AItem: string; var AHeight:Integer) of object;
  TJvPrintDrawItemEvent = procedure(Sender: TObject; ACanvas: TCanvas;
    ARect: TRect; AIndex: Integer; const AItem: string) of object;
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32)]
  {$ENDIF RTL230_UP}
  TJvPrint = class(TJvComponent)
  private
    FOnBeginPrint: TNotifyEvent;
    FOnFinishedPrint: TNotifyEvent;
    FOnProgress: TJvProgressEvent;
    FOnNextPage: TJvNextPageEvent;
    FOwnerDraw: Boolean;
    FOnMeasureItem: TJvPrintMeasureItemEvent;
    FOnDrawItem: TJvPrintDrawItemEvent;
  protected
    function MeasureItem(ACanvas: TCanvas; AIndex: Integer; AItem: string): Integer; virtual;
    procedure DrawItem(ACanvas: TCanvas; ARect: TRect; AIndex: Integer; const AItem: string); virtual;
  public
    function GetScaleX:Integer;
    function GetScaleY: Integer;
  published
    procedure Print(Value: TStringList);
    procedure PrintHTML(Value: TStrings);
    procedure PrintImage(Value: TBitmap; Style: TJvBitmapStyle);
    procedure Abort;
    property OwnerDraw: Boolean read FOwnerDraw write FOwnerDraw default False;
    property OnBeginPrint: TNotifyEvent read FOnBeginPrint write FOnBeginPrint;
    property OnFinishedPrint: TNotifyEvent read FOnFinishedPrint write FOnFinishedPrint;
    property OnProgress: TJvProgressEvent read FOnProgress write FOnProgress;
    property OnNextPage: TJvNextPageEvent read FOnNextPage write FOnNextPage;
    property OnMeasureItem: TJvPrintMeasureItemEvent read FOnMeasureItem write FOnMeasureItem;
    property OnDrawItem: TJvPrintDrawItemEvent read FOnDrawItem write FOnDrawItem;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  JvJCLUtils, JvJVCLUtils;

procedure TJvPrint.Abort;
begin
  Printer.Abort;
end;

procedure TJvPrint.Print(Value: TStringList);
var
  I, LineTop, LineHeight, PageNum: Integer;
  ARect: TRect;
begin
  //let's print
  if Assigned(FOnBeginPrint) then
    FOnBeginPrint(Self);
  LineTop := 0;
  Printer.BeginDoc;
  PageNum := 1;
  for I := 0 to Value.Count - 1 do
  begin
    if Assigned(FOnProgress) then
      FOnProgress(Self, I + 1, Value.Count);
    LineHeight := MeasureItem(Printer.Canvas, I, Value[I]);
    // JvHtControls:
    // AHeight := ItemHTHeight(ACanvas, AItem, GetScaleX)+1;
    if LineTop + LineHeight > Printer.PageHeight then
    begin
      LineTop := 0;
      Printer.NewPage;
      Inc(PageNum);
      if Assigned(FOnNextPage) then
        FOnNextPage(Self, PageNum);
    end;
    ARect := Rect(0, LineTop, Printer.PageWidth, LineTop + LineHeight);
    DrawItem(Printer.Canvas, ARect, I, Value[I]);
    LineTop := LineTop + LineHeight;
    // JvHTControls:
    // ARect.Bottom := ARect.Bottom - ARect.Top;
    // ItemHTDraw(ACanvas,ARect,[odReserved1],AItem, GetScaleX);
  end;
  Printer.EndDoc;
  if Assigned(FOnFinishedPrint) then
    FOnFinishedPrint(Self);
end;

procedure TJvPrint.PrintHTML(Value: TStrings);
var
  I, Line, PageNum: Integer;
  LHeight: Integer;
  LRect: TRect;
  LPixels: Integer;
begin
  //let's print
  if Assigned(FOnBeginPrint) then
    FOnBeginPrint(Self);
  Line := 0;
  Printer.BeginDoc;
  PageNum := 1;
  LPixels :=  GetScaleX;
  for I := 0 to Value.Count - 1 do
  begin
    if Assigned(FOnProgress) then
      FOnProgress(Self, I + 1, Value.Count);
    LHeight := HTMLTextHeight(Printer.Canvas, Value[I], LPixels) + 1;

    if Line + LHeight > Printer.PageHeight then
    begin
      Line := 0;
      Printer.NewPage;
      Inc(PageNum);
      if Assigned(FOnNextPage) then
        FOnNextPage(Self, PageNum);
    end;
    LRect := Rect(0,Line, Printer.PageWidth , LHeight);
    HTMLDrawText(Printer.Canvas,LRect,[odReserved1], Value[I], LPixels);
    Line := Line + LHeight;
  end;
  Printer.EndDoc;
  if Assigned(FOnFinishedPrint) then
    FOnFinishedPrint(Self);
end;

procedure TJvPrint.PrintImage(Value: TBitmap; Style: TJvBitmapStyle);
begin
  //let's print too :)
  if Assigned(FOnBeginPrint) then
    FOnBeginPrint(Self);
  case Style of
    bsNormal:
      with Printer do
      begin
        BeginDoc;
        Canvas.Draw(0, 0, Value);
        EndDoc;
      end;
    bsCentered:
      with Printer do
      begin
        BeginDoc;
        Canvas.Draw((PageWidth - Value.Width) div 2, (PageHeight - Value.Height) div 2, Value);
        EndDoc;
      end;
    bsStretched:
      with Printer do
      begin
        BeginDoc;
        CopyRectDIBits(Canvas, Rect(0, 0, PageWidth, PageHeight),
          Value, Rect(0, 0, Value.Width, Value.Height));
        EndDoc;
      end;
  end;
  if Assigned(FOnFinishedPrint) then
    FOnFinishedPrint(Self);
end;

function TJvPrint.GetScaleX: Integer;
begin
  Result := GetDeviceCaps(Printer.Handle, LogPixelsX);
end;

function TJvPrint.GetScaleY: Integer;
begin
  Result := GetDeviceCaps(Printer.Handle, LogPixelsY);
end;

function TJvPrint.MeasureItem(ACanvas: TCanvas; AIndex: Integer;
  AItem: string): Integer;
begin
  if OwnerDraw and Assigned(FOnMeasureItem) then
    FOnMeasureItem(Self, ACanvas, AIndex, AItem, Result)
  else
    Result := ACanvas.TextHeight(AItem);
end;

procedure TJvPrint.DrawItem(ACanvas: TCanvas; ARect: TRect;
  AIndex: Integer; const AItem: string);
begin
  if OwnerDraw and Assigned(FOnDrawItem) then
    FOnDrawItem(Self, ACanvas, ARect, AIndex, AItem)
  else
    ACanvas.TextOut(ARect.Left, ARect.Top, AItem);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
