{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPrint.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvPrint;

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Graphics, Printers,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, QPrinters,
  {$ENDIF VisualCLX}
  JvTypes, JvComponent;

type
  TJvPrint = class(TJvComponent)
  private
    FOnBeginPrint: TNotifyEvent;
    FOnFinishedPrint: TNotifyEvent;
    FOnProgress: TJvProgressEvent;
    FOnNextPage: TJvNextPageEvent;
  published
    procedure Print(Value: TStringList);
    procedure PrintImage(Value: TBitmap; Style: TJvBitmapStyle);
    procedure Abort;
    property OnBeginPrint: TNotifyEvent read FOnBeginPrint write FOnBeginPrint;
    property OnFinishedPrint: TNotifyEvent read FOnFinishedPrint write FOnFinishedPrint;
    property OnProgress: TJvProgressEvent read FOnProgress write FOnProgress;
    property OnNextPage: TJvNextPageEvent read FOnNextPage write FOnNextPage;
  end;

implementation

uses
  JvJCLUtils;

procedure TJvPrint.Abort;
begin
  Printer.Abort;
end;

procedure TJvPrint.Print(Value: TStringList);
var
  I, Line, PageNum: Integer;
begin
  //let's print
  if Assigned(FOnBeginPrint) then
    FOnBeginPrint(Self);
  Line := 0;
  Printer.BeginDoc;
  PageNum := 1;
  for I := 0 to Value.Count - 1 do
  begin
    if Assigned(FOnProgress) then
      FOnProgress(Self, I + 1, Value.Count);

    Line := Line + Printer.Canvas.TextHeight(Value[I]);
    if Line + Printer.Canvas.TextHeight(Value[I]) > Printer.PageHeight then
    begin
      Line := Printer.Canvas.TextHeight(Value[I]);
      Printer.NewPage;
      Inc(PageNum);
      if Assigned(FOnNextPage) then
        FOnNextPage(Self, PageNum);
    end;

    Printer.Canvas.TextOut(0, Line, Value[I]);
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
        {$IFDEF VCL}
        CopyRectDIBits(Canvas, Rect(0, 0, PageWidth, PageHeight),
          Value, Rect(0, 0, Value.Width, Value.Height));
        {$ENDIF VCL}
        {$IFDEF VisualCLX}
        Canvas.StretchDraw(Rect(0, 0, PageWidth, PageHeight), Value);
        {$ENDIF VisualCLX}
        EndDoc;
      end;
  end;
  if Assigned(FOnFinishedPrint) then
    FOnFinishedPrint(Self);
end;

end.

