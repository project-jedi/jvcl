{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: QExtDlgs.pas, released on 2003-12-05

The Initial Developer of the Original Code is André Snepvangers [asn att xs4all dott nl]
Copyright (C) 2003 André Snepvangers.
All Rights Reserved.

Contributor(s):

Known Issues:
----------------------------------------------------------------------------}
// $Id$

unit QExtDlgs;

interface

uses
  SysUtils, Classes,
  QDialogs, QPrinters, QConsts;

type
  TOpenPictureDialog = class(TOpenDialog)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property DefaultExt;
    property FileName;
    property Filter;
    property FilterIndex;
    property InitialDir;
    property Options;
    property Height;
    {$IFDEF MSWINDOWS}
    // property NativeFlags: Integer read FNativeFlags write FNativeFlags default 0;
    // property UseNativeDialog: Boolean read FUseNative write FUseNative default True;
    {$ENDIF MSWINDOWS}
    property Title;
    property OnClose;
    property OnCanClose;
    property OnFileAdd;
    property OnFilePreview;
    property OnFilterChange;
    property OnFolderChange;
    property OnHelpButtonClick;
    property OnSelected;
    property OnShow;
  end;

  TSavePictureDialog = class(TSaveDialog)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property DefaultExt;
    property FileName;
    property Filter;
    property FilterIndex;
    property InitialDir;
    property Options;
    property Height;
    {$IFDEF MSWINDOWS}
    // property NativeFlags: Integer read FNativeFlags write FNativeFlags default 0;
    // property UseNativeDialog: Boolean read FUseNative write FUseNative default True;
    {$ENDIF MSWINDOWS}
    property Title;
    property OnClose;
    property OnCanClose;
    property OnFileAdd;
    property OnFilePreview;
    property OnFilterChange;
    property OnFolderChange;
    property OnHelpButtonClick;
    property OnSelected;
    property OnShow;
  end;

  TPrinterSetupDialog = class(TComponent)
  private
    FOnShow: TNotifyEvent;
    FOnClose: TNotifyEvent;
  public
    function Execute: Boolean;
  published
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
  end;

implementation

const
  DefaultFilter =
    'Supported (*.bmp *.png *.xpm *.ico)|*.bmp;*.png;*.xpm;*.ico' + '|' +
    'Bitmaps (*.bmp *.png *.xpm)|*.bmp;*.png;*.xpm' + '|' +
    'Icons (*.ico)|*.ico';
  DefaultExt = '*.bmp';

//=== TOpenPictureDialog =====================================================

constructor TOpenPictureDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Options := Options + [ofPreview];
  DefaultExt := DefaultExt;
  Filter := DefaultFilter  + '|' + SDefaultFilter;
end;

//=== TSavePictureDialog =====================================================

constructor TSavePictureDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Options := Options + [ofPreview];
  DefaultExt := DefaultExt;
  Filter := DefaultFilter;
end;

//=== TPrinterSetupDialog ====================================================

function TPrinterSetupDialog.Execute: Boolean;
begin
  if Assigned(FOnShow)then
    FOnShow(Self);
  Result := Printer.ExecuteSetup;
  if Assigned(FOnClose) then
    FOnClose(Self);
end;

end.
