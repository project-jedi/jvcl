unit fAboutMe;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, jpeg, ExtCtrls, JvLabel, JvComponent, JvxCtrls;

type
  TfrmAboutMe = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Image1: TImage;
    Label2: TLabel;
    Memo1: TMemo;
    OKButton: TButton;
    JvHotLink2: TJvLabel;
    Label3: TLabel;
    JvHotLink1: TJvLabel;
    Version: TLabel;
    Comments: TLabel;
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
