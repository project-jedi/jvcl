unit fAboutMe;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, jpeg, ExtCtrls, JvLabel, JvComponent, JvExControls;

type
  TfrmAboutMe = class(TForm)
    Panel1: TPanel;
    Version: TLabel;
    Comments: TLabel;
    Label1: TLabel;
    Image1: TImage;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    OKButton: TButton;
    Label4: TLabel;
    JvHotLink1: TJvLabel;
    JvHotLink2: TJvLabel;
  private
    { Private declarations }
  public
    { Public declarations }

  end;

var
  frmAboutMe: TfrmAboutMe;

implementation

{$R *.dfm}

{ TfrmAboutMe }


end.
