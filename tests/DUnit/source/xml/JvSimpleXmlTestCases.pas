unit JvSimpleXmlTestCases;

interface
uses
  TestFramework, JvSimpleXml;

const
  SXmlNoGaps =
    '<xml>' +
    '<property name="value" another="value2"/>' +
    '<property name="value" another="value2">' +
    '</property>' +
    '</xml>';
  SXmlWithGaps =
    '<xml>' +
    '<property name = "value" another   =   "value2"  />' +
    '<property name = "value" another=   "value2">' +
    '</property>' +
    '<property name ="value" another  ="value2">' +
    '</property>' +
    '</xml>';
  SXMLInvalidName = '<doc><.doc></.doc></doc>';

type
  TTestJvSimpleXml = class(TTestCase)
  protected
    FXML: TJvSimpleXml;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure LoadMasterXML;
    procedure LoadFromFile(const Filename: string);
  published
    procedure TestWithGaps;
    procedure TestNoGaps;
    procedure JamesClarkTests;
  end;

implementation
uses
  SysUtils;

{ TTestJvSimpleXml }

procedure TTestJvSimpleXml.JamesClarkTests;
begin
  LoadMasterXML;
end;

procedure TTestJvSimpleXml.LoadFromFile(const Filename: string);
begin
  FXML.LoadFromFile(ExpandUNCFileName(Filename));
end;

procedure TTestJvSimpleXml.LoadMasterXML;
var
  AXML: TJvSimpleXML;
  i: integer;
  RootFolder, S: string;

  function FailExpected(const S: string): boolean;
  begin
    // (p3) "not-wf" and "invalid" are expected to raise Exceptions (if the parser works according to definition), so we need
    // to check for that
    Result := (S = 'not-wf') or (S = 'invalid')
  end;

begin
  // load James Clark's XML test suite files as defined in the xmltest.xml file
  AXML := TJvSimpleXML.Create(nil);
  try
    RootFolder := ExtractFilePath(ParamStr(0)) + 'xmltest\';
    Check(FileExists(RootFolder + 'xmltest.xml'), 'The xmltest folder must be a subfolder of the applicaiton folder!');
    AXML.LoadFromFile(RootFolder + 'xmltest.xml');
    // AXML.Root.Properties.ItemNamed['PROFILE'].Value
//    StartExpectingException(ETestFailure);
    for i := 0 to AXML.Root.Items.Count - 1 do
    begin
      if (AXML.Root.Items[i].Properties.Count = 0) or (AXML.Root.Items[i].Properties.ItemNamed['URI'] = nil) then
        Continue;
      S := RootFolder + StringReplace(AXML.Root.Items[i].Properties.ItemNamed['URI'].Value, '/', '\', [rfReplaceAll]);
      if FileExists(S) then
      try
        LoadFromFile(S);
        // check whether this test should have failed, but didn't
        with AXML.Root.Items[i].Properties do
          Check(not FailExpected(ItemNamed['TYPE'].Value),
            Format('Type: "%s", ID: "%s" Section: "%s > %s" Error: "%s"',
            [ItemNamed['TYPE'].Value, ItemNamed['ID'].Value, ItemNamed['SECTIONS'].Value, AXML.Root.Items[i].Value,
            'This test should have failed, but didn''t.']));
      except
        on E: TJvSimpleXmlInvalid do
          with AXML.Root.Items[i].Properties do
            // check whether this is an expected failure and in that case, don't raise exception
            Check(FailExpected(ItemNamed['TYPE'].Value), Format('Type: "%s", ID: "%s" Section: "%s > %s" Error: "%s"',
              [ItemNamed['TYPE'].Value, ItemNamed['ID'].Value, ItemNamed['SECTIONS'].Value, AXML.Root.Items[i].Value,
              E.Message]));
      end;
    end;
    //    StopExpectingException('');
  finally

    AXML.Free;
  end;

end;

procedure TTestJvSimpleXml.SetUp;
begin
  FXML := TJvSimpleXml.Create(nil);
  inherited;
end;

procedure TTestJvSimpleXml.TearDown;
begin
  FXML.Free;
  inherited;
end;

procedure TTestJvSimpleXml.TestNoGaps;
begin
  FXML.LoadFromString(SXmlNoGaps);
end;

procedure TTestJvSimpleXml.TestWithGaps;
begin
  FXML.LoadFromString(SXmlWithGaps);
end;

initialization
  RegisterTests([TTestJvSimpleXml.Suite]);
end.

