unit MDIPluginFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

type
  TfrmMDIChild = class(TForm)
    RichEdit1: TRichEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

procedure TfrmMDIChild.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
   Action := caFree;
end;

end.
