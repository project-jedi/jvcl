unit fIniSections;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TIniSections = class(TForm)
    Button2: TButton;
    Button1: TButton;
    ListBox1: TListBox;
    Label1: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  IniSections: TIniSections;

implementation

{$R *.DFM}

end.
