{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

unit MainFrm;

interface

uses
  QWindows, QMessages, SysUtils, Classes, Types, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, JvQExStdCtrls, JvQCheckBox, JvQRadioButton, QButtons,
  QExtCtrls, JvQEdit, JvQExControls, JvQComponent, JvQLabel, QImgList;

type
  TForm1 = class(TForm)
    chkShowToolTips: TJvCheckBox;
    edPrefix: TJvEdit;
    rbOption1: TJvRadioButton;
    rbOption2: TJvRadioButton;
    rbOption3: TJvRadioButton;
    lblOption1: TJvLabel;
    lblOption2: TJvLabel;
    lblOption3: TJvLabel;
    pnlInfo: TPanel;
    lblInfo: TJvLabel;
    lblPrefix: TJvLabel;
    ImageList1: TImageList;
    chkShowPrefix: TJvCheckBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.xfm}

end.
