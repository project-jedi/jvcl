unit JvWindowsLister;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, JvComponent, JvCaptionPanel;

type
  TJvWindowsListerForm = class(TForm)
    JvCaptionPanel1: TJvCaptionPanel;
    ListBox1: TListBox;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  JvWindowsListerForm: TJvWindowsListerForm;

implementation

uses
  JvFunctions;

{$R *.dfm}

procedure TJvWindowsListerForm.Button1Click(Sender: TObject);
var
  S : TStringlist;
  i : integer;
begin
  S := TStringlist.Create;
  ListBox1.Items.BeginUpdate;
  try
    ListBox1.Items.Clear;
    GetVisibleWindows(S);
    for i := 0 to S.Count - 1 do
      ListBox1.Items.Add(Format('%s (%d)',[S[i],integer(S.Objects[i])]));
  finally
    ListBox1.Items.EndUpdate;
    S.Free;
  end;
end;

end.
