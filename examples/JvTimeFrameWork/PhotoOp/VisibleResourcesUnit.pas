unit VisibleResourcesUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, checklst;

type
  TVisibleResources = class(TForm)
    ResourcesCheckList: TCheckListBox;
    OKButton: TBitBtn;
    CancelButton: TBitBtn;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  VisibleResources: TVisibleResources;

implementation

Uses PhotoOpUnit;

{$R *.DFM}

procedure TVisibleResources.FormShow(Sender: TObject);
var
  I : Integer;
begin
  // Check the template and check any resources currently
  // visible in the grid
  With PhotoOpMain.JvTFDays1.Template do
    For I := 0 to ResourcesCheckList.Items.Count - 1 do
      ResourcesCheckList.Checked[I] :=
        CompNames.IndexOf(ResourcesCheckList.Items[I]) > -1;
end;

procedure TVisibleResources.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  I : Integer;
begin
  If ModalResult = mrOK Then
  begin
      // First, clear the old resources from the template
      PhotoOpMain.JvTFDays1.Template.CompNames.Clear;
      PhotoOpMain.JvTFWeeks1.SchedNames.Clear;
      PhotoOpMain.JvTFMonths1.SchedNames.Clear;

      // Next, add the new resources to the template
      For I := 0 to ResourcesCheckList.Items.Count - 1 do
         If ResourcesCheckList.Checked[I] Then
         begin
             PhotoOpMain.JvTFDays1.Template.CompNames.Add(ResourcesCheckList.Items[I]);
             PhotoOpMain.JvTFWeeks1.SchedNames.Add(ResourcesCheckList.Items[I]);
             PhotoOpMain.JvTFMonths1.SchedNames.Add(ResourcesCheckList.Items[I]);
         end;
  end;
end;

end.
