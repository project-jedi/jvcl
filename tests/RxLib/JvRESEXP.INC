{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{*******************************************************}

{$I RX.INC}

{ String constants }

{$IFDEF RX_D3}
resourcestring
{$ELSE}
const
{$ENDIF}
  sExpertName = 'Project Resource Expert';
{$IFDEF RX_D4}
  sMenuItemCaption = 'Resources';
{$ELSE}
  sMenuItemCaption = 'Pro&ject Resources';
{$ENDIF}
  sCannotRename = 'A resource named "%s" already exists';
  sInvalidName = '"%s" is not a valid resource name';
  sInvalidType = '"%s" is not a valid resource type';
  sNewBitmap = 'New Bitmap';
  sNewResource = 'New Binary Resource';
  sResType = '&Resource Type:';
  sAllFilesFilter = 'All Files (*.*)|*.*';
  sCursorFilesFilter = 'Windows Cursors (*.cur)|*.cur';
  sAniCursorFilesFilter = 'Animated Cursors (*.ani)|*.ani';
  sIconFilesFilter = 'Icon Files (*.ico)|*.ico';

