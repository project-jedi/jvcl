unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvExStdCtrls, JvCheckBox, JvRadioButton, Buttons,
  ExtCtrls, JvEdit, JvExControls, JvComponent, JvLabel, ImgList;

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

{$R *.dfm}

end.
