unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, CSIntf;

type
  TForm1 = class(TForm)
    Panel8: TPanel;
    Button1: TButton;
    Panel1: TPanel;
    Panel7: TPanel;
    Panel6: TPanel;
    Panel5: TPanel;
    Panel4: TPanel;
    Panel3: TPanel;
    Panel2: TPanel;
    Panel9: TPanel;
    CSGlobalObject1: TCSGlobalObject;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

uses
  JvConsts;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  LoadOLEDragCursors;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Panel4.Cursor := crMultiDragLink;
  Panel5.Cursor := crDragAlt;
  Panel6.Cursor := crMultiDragAlt;
  Panel7.Cursor := crMultiDragLinkAlt;
end;

end.

