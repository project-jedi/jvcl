{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: FrmInstall.pas, released on 2004-04-06.

The Initial Developer of the Original Code is Andreas Hausladen
(Andreas dott Hausladen att gmx dott de)
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit FrmInstall;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls,
  ShellAPI,
  JVCL3Install, JVCLData, PackageUtils, Compile, Utils;

type
  TFrameInstall = class(TFrame)
    LblTarget: TLabel;
    ProgressBarTarget: TProgressBar;
    ProgressBarCompile: TProgressBar;
    LblInfo: TLabel;
    RichEditLog: TRichEdit;
    LblOpenFile: TLabel;
    BtnDetails: TButton;
    procedure RichEditLogSelectionChange(Sender: TObject);
    procedure BtnDetailsClick(Sender: TObject);
    procedure LblOpenFileClick(Sender: TObject);
  private
    FInitializing: Boolean;
    FInstaller: TInstaller;
    procedure Init;
  protected
    property Installer: TInstaller read FInstaller;
  private
    FPositionTarget: Integer;
    FPositionProject: Integer;
    FFinished: Boolean;
    FAborted: Boolean;
    procedure EvProgress(Sender: TObject; const Text: string;
      Position, Max: Integer; Kind: TProgressKind);
    procedure EvCaptureLine(const Text: string; var Aborted: Boolean);
    procedure EvIdle(Sender: TObject);
  public
    class function Build(Installer: TInstaller; Client: TWinControl): TFrameInstall;
    procedure Execute;

    property Aborted: Boolean read FAborted write FAborted;
  end;

implementation

end.