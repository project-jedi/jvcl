{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvGridPrinter.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1 dott verhoeven att wxs dott nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove att slcdug dott org].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvGridPrinter;

{$I jvcl.inc}

interface

uses
  Windows, Controls, Forms, Grids, Printers, SysUtils, Classes;

type
  TJvPrintMode = (pmPrint, pmPreview, pmPageCount);

  TJvPrintOptions = class(TPersistent)
  private
    FJobTitle: string;
    FPageTitle: string;
    FPageTitleMargin: Cardinal;
    FCopies: Cardinal;
    FPreviewPage: Cardinal;
    FBorderStyle: TBorderStyle;
    FLeftPadding: Cardinal;
    FMarginBottom: Cardinal;
    FMarginLeft: Cardinal;
    FMarginTop: Cardinal;
    FMarginRight: Cardinal;
    FPageFooter: string;
    FDateFormat: string;
    FTimeFormat: string;
    FHeaderSize: Cardinal;
    FFooterSize: Cardinal;
    FOrientation: TPrinterOrientation;
    FLogo: string;
  published
    property Orientation: TPrinterOrientation read FOrientation write FOrientation;
    property JobTitle: string read FJobTitle write FJobTitle;
    property PageTitle: string read FPageTitle write FPageTitle;
    property Logo: string read FLogo write FLogo;
    property PageTitleMargin: Cardinal read FPageTitleMargin write FPageTitleMargin;
    property PageFooter: string read FPageFooter write FPageFooter;
    property HeaderSize: Cardinal read FHeaderSize write FHeaderSize;
    property FooterSize: Cardinal read FFooterSize write FFooterSize;
    property DateFormat: string read FDateFormat write FDateFormat;
    property TimeFormat: string read FTimeFormat write FTimeFormat;
    property Copies: Cardinal read FCopies write FCopies default 1;
    property PreviewPage: Cardinal read FPreviewPage write FPreviewPage;
    property BorderStyle: TBorderStyle read FBorderStyle write FBorderStyle;
    property Leftpadding: Cardinal read FLeftPadding write FLeftPadding;
    property MarginBottom: Cardinal read FMarginBottom write FMarginBottom;
    property MarginLeft: Cardinal read FMarginLeft write FMarginLeft;
    property MarginTop: Cardinal read FMarginTop write FMarginTop;
    property MarginRight: Cardinal read FMarginRight write FMarginRight;
  end;

  TJvGridPrinter = class(TComponent)
  private
    FPrintOptions: TJvPrintOptions;
    FGrid: TStringGrid;
    FNumbersAlright: Boolean;
    FNumberFormat: string;
    FWordWrap: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Preview: Boolean;
  published
    property PrintOptions: TJvPrintOptions read FPrintOptions write FPrintOptions;
    property Grid: TStringGrid read FGrid write FGrid;
    property WordWrap: Boolean read FWordWrap write FWordWrap default True;
    property NumbersAlright: Boolean read FNumbersAlright write FNumbersAlright default True;
    property NumberFormat: string read FNumberFormat write FNumberFormat;
  end;

implementation

uses
  JvGridPreviewForm, JvTypes, JvResources;

constructor TJvGridPrinter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPrintOptions := TJvPrintOptions.Create;
  FPrintOptions.PageFooter := RsPrintOptionsPageFooter;
  FPrintOptions.DateFormat := RsPrintOptionsDateFormat;
  FPrintOptions.TimeFormat := RsPrintOptionsTimeFormat;
  FPrintOptions.HeaderSize := 14;
  FPrintOptions.FooterSize := 8;
  FPrintOptions.PreviewPage := 1;
  FNumbersAlright := True;
  FNumberFormat := '%.2f';
  FWordWrap := True;
end;

destructor TJvGridPrinter.Destroy;
begin
  FPrintOptions.Free;
  inherited Destroy;
end;

function TJvGridPrinter.Preview: Boolean;
var
  Preview: TJvGridPreviewForm;
begin
  if Assigned(FGrid) then
  begin
    Preview := TJvGridPreviewForm.Create(Application);
    Preview.GridPrinter := Self;
    Preview.Grid := Grid;
    Preview.ShowModal;
    Preview.Free;
    Result := True;
  end
  else
    Result := False;
end;

end.
