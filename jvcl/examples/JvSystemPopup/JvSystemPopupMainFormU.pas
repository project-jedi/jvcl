unit JvSystemPopupMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Menus, JvSystemPopup, JvTypes, JvComponent;

type
  TJvSystemPopupMainForm = class(TForm)
    JvSystemPopup1: TJvSystemPopup;
    PopupMenu1: TPopupMenu;
    firstone1: TMenuItem;
    secondone1: TMenuItem;
    thirdone1: TMenuItem;
    RadioGroup1: TRadioGroup;
    SubMenu11: TMenuItem;
    SubMenu12: TMenuItem;
    SubMenu21: TMenuItem;
    SubMenu31: TMenuItem;
    N1: TMenuItem;
    SubMenu22: TMenuItem;
    SubMenuItem11: TMenuItem;
    SubMenuItem21: TMenuItem;
    SubMenuItem31: TMenuItem;
    SubMenuItem41: TMenuItem;
    SubMenuItem51: TMenuItem;
    SubMenuItem1: TMenuItem;
    SubMenuItem71: TMenuItem;
    SubMenuItem81: TMenuItem;
    SubMenuItem91: TMenuItem;
    procedure firstone1Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
  end;

var
  JvSystemPopupMainForm: TJvSystemPopupMainForm;

implementation

{$R *.DFM}

procedure TJvSystemPopupMainForm.firstone1Click(Sender: TObject);
begin
  showmessage((sender as TMEnuItem).caption);
end;

procedure TJvSystemPopupMainForm.RadioGroup1Click(Sender: TObject);
begin
  case self.RadioGroup1.itemindex of
    0 : self.JvSystemPopup1.Position := ppNone;
    1 : self.JvSystemPopup1.Position := ppApplication;
    2 : self.JvSystemPopup1.Position := ppForm;
  end;
end;

end.
