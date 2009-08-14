{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.delphi-jedi.org

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit CsvDataSourceUnicodeDemoFm;

{ Demo by Warren Postma, warrenpstma@hotmail.com }

{$I jvcl.inc}  // JVCL source code needs these compiler version constants

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DB, Grids, DBGrids, ExtCtrls, DBCtrls, StdCtrls,
  ComCtrls, JvCSVParse, JvCsvData;


type
  TCsvDataSourceForm = class(TForm)
    DataSource1: TDataSource;
    DBNavigator1: TDBNavigator;
    DBGrid1: TDBGrid;
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    Button3: TButton;
    JvCsvDataSet1: TJvCsvDataSet;
    Label2: TLabel;
    ComboBox1: TComboBox;
    RichEdit1: TRichEdit;
    Button4: TButton;
    OpenDialog1: TOpenDialog;
    JvCsvDataSet1NAME: TWideStringField;
    JvCsvDataSet1ADDRESS: TWideStringField;
    JvCsvDataSet1ADDRESS2: TWideStringField;
    JvCsvDataSet1TELEPHONE: TWideStringField;
    JvCsvDataSet1AGE: TIntegerField;
    JvCsvDataSet1LASTPHONECALL: TDateTimeField;
    JvCsvDataSet1PRIVATENUMBER: TBooleanField;
    FontDialog1: TFontDialog;
    Button5: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ComboBox1Change(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  public
  end;

var
  CsvDataSourceForm: TCsvDataSourceForm;

implementation

{$R *.dfm}

uses JclSysInfo;

procedure TCsvDataSourceForm.Button2Click(Sender: TObject);
begin
  // OutputDebugString(PChar(JvCsvDataSet1.GetCsvHeader)); // neat trick. reads first line of file only.

  JvCsvDataSet1.Flush; // flush current contents to disk. okay, maybe it should be called save,
                       // then you wouldn't see jokes in the code about how this component has
                       // been tested, and shown to last for over two thousand flushes. <grin>
end;

procedure TCsvDataSourceForm.Button1Click(Sender: TObject);
begin
   Memo1.Clear;
   JvCsvDataSet1.AssignToStrings(Memo1.Lines);
end;

procedure TCsvDataSourceForm.Button3Click(Sender: TObject);
begin
   JvCsvDataSet1.Active := False;
   JvCsvDataSet1.AssignFromStrings(Memo1.Lines);
   JvCsvDataSet1.Active := True;
end;

procedure TCsvDataSourceForm.FormCreate(Sender: TObject);
begin

   // second component tests what happens if you don't set up the CsvDef field

//  JvCsvDataSet2.Active := True; // ensure it's opened when the app is started.
end;

procedure TCsvDataSourceForm.FormShow(Sender: TObject);
var
 fontfilename:String;
begin
   JvCsvDataSet1.Active := True; // ensure it's opened when the app is started.

   { Handy-Dandy International Font Plug&Play Code: }
  fontfilename := IncludeTrailingPathDelimiter( GetFontsFolder)+'arialuni.ttf';
   if FileExists(fontfilename) then begin
      DBGrid1.Font.Name := 'Arial Unicode MS';
      DBGrid1.Font.Size := 12;

   end;

  {$ifdef COMPILER12_UP}
        RichEdit1.Lines.Add('Since you have compiled this on Delphi 2009, this demo should work. You should be able to cut and paste Unicode text from web pages and documents and paste it into any of the text fields in this demo. ');
        RichEdit1.Lines.Add('If you can not see a particular script you might not have all the fonts and international-support installed in your operating system.');
  {$else}
      { This demo either demonstrates something you can DO, or something you CAN NOT YET DO.
         It compiles fine but only works on delphi 2009. This is part of the purpose of this demo,
         to show what works and what doesn't work, on various versions of delphi.
      }
        RichEdit1.Lines.Add('Warning: You can compile this demo on your OLD version of Delphi but all you can do is demonstrate the non-functional nature of this version of delphi.  ');
        RichEdit1.Lines.Add('Don''t report it as a bug in JVCL. The VCL/RTL do not have enough Unicode support for this demo to work, except in Delphi 2009.');
        RichEdit1.Font.Color := clRed;
  {$endif}

end;

procedure TCsvDataSourceForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if DataSource1.State = dsEdit then
    JvCsvDataSet1.Post;
end;

procedure TCsvDataSourceForm.ComboBox1Change(Sender: TObject);
begin
  // kind of stupid hack, but hey:
   JvCsvDataSet1LASTPHONECALL.DisplayFormat := String(JvAnsiStrStrip(AnsiString(ComboBox1.Text)));
end;

procedure TCsvDataSourceForm.Button4Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    Memo1.Lines.LoadFromFile(OpenDialog1.FileName);
end;

procedure TCsvDataSourceForm.Button5Click(Sender: TObject);
begin
 FontDialog1.Font.Name := DBGrid1.Font.Name;
  FontDialog1.Font.Size := DBGrid1.Font.Size;
 if FontDialog1.Execute  then begin { ..(Self.Handle) }
    DBGrid1.Font.Name := FontDialog1.Font.Name;
    DBGrid1.Font.Size := FontDialog1.Font.Size;
 end;

end;

end.