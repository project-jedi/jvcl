unit JvWindowsTitleMainFomU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls;

type
  TJvWindowsTitleMainForm = class(TForm)
    Button1: TButton;
    ListBox1: TListBox;
    procedure Button1Click(Sender: TObject);
  end;

var
  JvWindowsTitleMainForm: TJvWindowsTitleMainForm;

implementation
uses
  JvFunctions;

{$R *.DFM}

procedure TJvWindowsTitleMainForm.Button1Click(Sender: TObject);
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
