unit JvOLBar_Test;

interface
uses
  Classes, TestFramework, JvOutlookBar;

type
  TJvOutlookBarTests = class(TTestCase)
  private
    FBar:TJvOutlookbar;
    function GetBar: TJvOutlookbar;
    procedure AddPages(PageCount,ButtonCount:integer);
    procedure AddButtons(const Page:TJvOutlookBarPage;Count:integer);
  protected
    property Bar:TJvOutlookbar read GetBar;
  published
    procedure TestAddDeletePages;
    procedure TestAddDeleteButtons;
    procedure TestPropertiesChange;
    procedure TestPagesAndButtonsAssign;
    procedure TestAssign;
  end;

implementation
uses
  Graphics, SysUtils;

{ TJvOutlookBarTests }

function TJvOutlookBarTests.GetBar: TJvOutlookbar;
begin
  if FBar = nil then
    FBar := TJvOutlookbar.Create(nil);
  Result := FBar;
end;


procedure TJvOutlookBarTests.TestAddDeleteButtons;
var i:integer;
begin
  Bar.Pages.Clear;
  CheckEquals(0,Bar.Pages.Count,'Page count not zero!');
  AddPages(10,10);
  CheckEquals(10,Bar.Pages.Count,'Page count does not match!');
  i := 0;
  while i < Bar.Pages.Count do
  begin
    CheckEquals(10,Bar.Pages[i].Buttons.Count,Format('Button count does not match (i is %d)',[i]));
    Bar.Pages[i].Buttons.Clear;
    CheckEquals(0,Bar.Pages[i].Buttons.Count,'Button count not zero!');
    Inc(i);
  end;
  Bar.Pages.Clear;
  CheckEquals(0,Bar.Pages.Count,'Page count not zero!');
end;

procedure TJvOutlookBarTests.TestAddDeletePages;
begin
  Bar.Pages.Clear;
  CheckEquals(0,Bar.Pages.Count,'Page count not zero!');
  AddPages(10,0);
  CheckEquals(10,Bar.Pages.Count,'Page count does not match!');
  Bar.Pages.Clear;
  CheckEquals(0,Bar.Pages.Count,'Page count not zero!');
end;

procedure TJvOutlookBarTests.TestPropertiesChange;
begin
  Bar.Width := 100;
  CheckEquals(100,Bar.Width,'Unable to set Width');
  Bar.Height := 100;
  CheckEquals(100,Bar.Height,'Unable to set Height');
  Bar.Color := clBtnFace;
  CheckEquals(clBtnFace,Bar.Color,'Unable to set Color');
  // etc...
end;

procedure TJvOutlookBarTests.TestPagesAndButtonsAssign;
var FControl:TJvOutlookBar;i:integer;
begin
  FControl := TJvOutlookBar.Create(nil);
  try
    AddPages(10,10);
    // code here to add pages, buttons, change properties
    FControl.Pages.Assign(bar.Pages);
    // FControl.Assign(FControl2);
    // code here to check that everything matches
    CheckEquals(Bar.Pages.Count,FControl.Pages.Count,'Page count does not match!');
    for i := 0 to Bar.Pages.Count - 1 do
      CheckEquals(Bar.Pages[i].Buttons.Count,FControl.Pages[i].Buttons.Count, 'Button count does not match.');
  finally
    FreeAndNil(FControl);
  end;
end;

procedure TJvOutlookBarTests.AddButtons(const Page: TJvOutlookBarPage;
  Count: integer);
var i:integer;B:TJvOutlookBarButton;
begin
  Check(Page is TJvOutlookbarPage,'Must pass valid TJvOutlookBarPage to AddButtons!');
  for i := 1 to Count do
  begin
    B := Page.Buttons.Add;
    B.Caption := Format('%s_Button %d',[Page.Caption,Page.Buttons.Count]);
    B.ImageIndex := Random(100);
    B.Tag := Random(100);
  end;
end;

procedure TJvOutlookBarTests.AddPages(PageCount,ButtonCount:integer);
var i:integer;P:TJvOutlookBarPage;
begin
  for i := 1 to PageCount do
  begin
    P := Bar.Pages.Add;
    P.Caption := Format('Page %d',[Bar.Pages.Count]);
    AddButtons(P,ButtonCount);
  end;
end;

procedure TJvOutlookBarTests.TestAssign;
var FControl:TJvOutlookBar;i:integer;
begin
  // this should fail since Assign isn't implemented in JvOutlookBar (yet)
  FControl := TJvOutlookBar.Create(nil);
  try
    FControl.Assign(Bar);
  finally
    FreeAndNil(FControl);
  end;
end;

initialization
  RegisterTests([TJvOutlookBarTests.Suite]);

end.
