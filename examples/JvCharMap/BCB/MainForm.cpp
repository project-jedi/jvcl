/******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2004 Project JEDI

 Original author: Olivier Sannier (obones att altern dott org)

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************/
//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "MainForm.h"
#include "JvClipbrd.hpp"
#include <typinfo.hpp>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvColorCombo"
#pragma link "JvCombobox"
#pragma link "JvExStdCtrls"
#pragma link "JvCharMap"
#pragma link "JvClipbrd"
#pragma resource "*.dfm"
TfrmMain *frmMain;

WideString GetTypeString1(wchar_t AChar)
{
  unsigned short ACharInfo=0;
  WideString Result = "";

  if (GetStringTypeExW(LOCALE_USER_DEFAULT, CT_CTYPE1, &AChar, 1, &ACharInfo))
  {
    if (ACharInfo & C1_UPPER == C1_UPPER)
      Result = Result + ",C1_UPPER";
    if (ACharInfo & C1_LOWER == C1_LOWER)
      Result = Result + ",C1_LOWER";
    if (ACharInfo & C1_DIGIT == C1_DIGIT)
      Result = Result + ",C1_DIGIT";
    if (ACharInfo & C1_SPACE == C1_SPACE)
      Result = Result + ",C1_SPACE";
    if (ACharInfo & C1_PUNCT == C1_PUNCT)
      Result = Result + ",C1_PUNCT";
    if (ACharInfo & C1_CNTRL == C1_CNTRL)
      Result = Result + ",C1_CNTRL";
    if (ACharInfo & C1_BLANK == C1_BLANK)
      Result = Result + ",C1_BLANK";
    if (ACharInfo & C1_XDIGIT == C1_XDIGIT)
      Result = Result + ",C1_XDIGIT";
    if (ACharInfo & C1_ALPHA == C1_ALPHA)
      Result = Result + ",C1_ALPHA";
  }
  if (Result != WideString(L""))
    Result = Result.SubString(2, MaxInt);

  return Result;
}

WideString GetTypeString2(wchar_t AChar)
{
  unsigned short ACharInfo=0;
  WideString Result = "";

  if (GetStringTypeExW(LOCALE_USER_DEFAULT, CT_CTYPE2, &AChar, 1, &ACharInfo))
  {
    if (ACharInfo & C2_LEFTTORIGHT == C2_LEFTTORIGHT)
      Result = Result + ",C2_LEFTTORIGHT";
    if (ACharInfo & C2_RIGHTTOLEFT == C2_RIGHTTOLEFT)
      Result = Result + ",C2_RIGHTTOLEFT";
    if (ACharInfo & C2_EUROPENUMBER == C2_EUROPENUMBER)
      Result = Result + ",C2_EUROPENUMBER";
    if (ACharInfo & C2_EUROPESEPARATOR == C2_EUROPESEPARATOR)
      Result = Result + ",C2_EUROPESEPARATOR";
    if (ACharInfo & C2_EUROPETERMINATOR == C2_EUROPETERMINATOR)
      Result = Result + ",C2_EUROPETERMINATOR";
    if (ACharInfo & C2_ARABICNUMBER == C2_ARABICNUMBER)
      Result = Result + ",C2_ARABICNUMBER";
    if (ACharInfo & C2_COMMONSEPARATOR == C2_COMMONSEPARATOR)
      Result = Result + ",C2_COMMONSEPARATOR";
    if (ACharInfo & C2_BLOCKSEPARATOR == C2_BLOCKSEPARATOR)
      Result = Result + ",C2_BLOCKSEPARATOR";
    if (ACharInfo & C2_SEGMENTSEPARATOR == C2_SEGMENTSEPARATOR)
      Result = Result + ",C2_SEGMENTSEPARATOR";
    if (ACharInfo & C2_WHITESPACE == C2_WHITESPACE)
      Result = Result + ",C2_WHITESPACE";
    if (ACharInfo & C2_OTHERNEUTRAL == C2_OTHERNEUTRAL)
      Result = Result + ",C2_OTHERNEUTRAL";
  }
  if (Result != WideString(L""))
    Result = Result.SubString(2, MaxInt);

  return Result;
}

WideString GetTypeString3(wchar_t AChar)
{
  WideString Result = "";
  unsigned short ACharInfo = 0;

  if (GetStringTypeExW(LOCALE_USER_DEFAULT, CT_CTYPE3, &AChar, 1, &ACharInfo))
  {
    if (ACharInfo & C3_NONSPACING == C3_NONSPACING)
      Result = Result + ",C3_NONSPACING";
    if (ACharInfo & C3_DIACRITIC == C3_DIACRITIC)
      Result = Result + ",C3_DIACRITIC ";
    if (ACharInfo & C3_VOWELMARK == C3_VOWELMARK)
      Result = Result + ",C3_VOWELMARK";
    if (ACharInfo & C3_SYMBOL == C3_SYMBOL)
      Result = Result + ",C3_SYMBOL";
    if (ACharInfo & C3_KATAKANA == C3_KATAKANA)
      Result = Result + ",C1_PUNCT";
    if (ACharInfo & C3_HIRAGANA == C3_HIRAGANA)
      Result = Result + ",C3_HIRAGANA";
    if (ACharInfo & C3_HALFWIDTH == C3_HALFWIDTH)
      Result = Result + ",C3_HALFWIDTH";
    if (ACharInfo & C3_FULLWIDTH == C3_FULLWIDTH)
      Result = Result + ",C3_FULLWIDTH";
    if (ACharInfo & C3_IDEOGRAPH == C3_IDEOGRAPH)
      Result = Result + ",C3_IDEOGRAPH";
    if (ACharInfo & C3_KASHIDA == C3_KASHIDA)
      Result = Result + ",C3_KASHIDA";
    if (ACharInfo & C3_ALPHA == C3_ALPHA)
      Result = Result + ",C3_ALPHA";
  }
  if (Result != WideString(L""))
    Result = Result.SubString(2, MaxInt);

  return Result;
}

void __fastcall TfrmMain::cbColorChange(TObject *Sender)
{
  if (JM != NULL)
    JM->Color = cbColor->ColorValue;
}

void __fastcall TfrmMain::cbFontChange(TObject *Sender)
{
  if (JM != NULL)
    JM->Font->Name = cbFont->FontName;
}

// To fill the filter combo box, we need to enumerate and retrieve
// all names of the TJvCharMapUnicodeFilter enumeration.
// Please see the JvInspector demo for more information on this
// sort of construct required to get runtime type information
class TJvCharMapUnicodeFilterTypeInfoHelper : public TPersistent
{
  private:
    TJvCharMapUnicodeFilter FTJvCharMapUnicodeFilterProp;
  __published:
    __property TJvCharMapUnicodeFilter TJvCharMapUnicodeFilterProp =
      { read = FTJvCharMapUnicodeFilterProp};
  public:
    static PTypeInfo TypeInfo(void)
      {
        PTypeInfo ClassTypeInfo;
        PPropInfo PropertyInfo;
        PTypeInfo PropertyTypeInfo;

        ClassTypeInfo = __typeinfo(TJvCharMapUnicodeFilterTypeInfoHelper);

        PropertyInfo = GetPropInfo(ClassTypeInfo, "TJvCharMapUnicodeFilterProp");

        PropertyTypeInfo = *(PropertyInfo->PropType);

        return PropertyTypeInfo;
      }

    static TJvCharMapUnicodeFilter Low(void)
      {
        PTypeData data;
        data = GetTypeData(TypeInfo());

        return static_cast<TJvCharMapUnicodeFilter>(data->MinValue);
      }

    static TJvCharMapUnicodeFilter High(void)
      {
        PTypeData data;
        data = GetTypeData(TypeInfo());

        return static_cast<TJvCharMapUnicodeFilter>(data->MaxValue);
      }
};

void __fastcall TfrmMain::FillFilter()
{
  cbFilter->Items->BeginUpdate();
  try
  {
    cbFilter->Items->Clear();
    
    for(int i=TJvCharMapUnicodeFilterTypeInfoHelper::Low(); i<=TJvCharMapUnicodeFilterTypeInfoHelper::High(); i++)
      cbFilter->Items->Add(GetEnumName(TJvCharMapUnicodeFilterTypeInfoHelper::TypeInfo(), i));
  }
  __finally
  {
    cbFilter->Items->EndUpdate();
  }
  cbFilter->ItemIndex = static_cast<int>(JM->CharRange->Filter);
}

void __fastcall TfrmMain::FillLocales()
{
  int I;

  cbLocales->Items->BeginUpdate();
  try
  {
    cbLocales->Items->Clear();
    cbLocales->Items->AddObject("System Default",reinterpret_cast<TObject*>(LOCALE_SYSTEM_DEFAULT));
    cbLocales->Items->AddObject("User Default",reinterpret_cast<TObject*>(LOCALE_USER_DEFAULT));
    for (I = 0; I < Languages()->Count; I++)
      cbLocales->Items->AddObject(Languages()->Name[I], reinterpret_cast<TObject*>(Languages()->LocaleID[I]));
  }
  __finally
  {
    cbLocales->Items->EndUpdate();
  }
  cbLocales->ItemIndex = cbLocales->Items->IndexOfObject(reinterpret_cast<TObject*>(JM->Locale));
  cbLocales->Enabled = Win32Platform != VER_PLATFORM_WIN32_NT;
}

void __fastcall TfrmMain::DoJMSelectChar(TObject *Sender, WideChar AChar)
{
  DisplayInfo(AChar);
}

void __fastcall TfrmMain::DoJMResize(TObject *Sender)
{
  JM->Left = (ClientWidth - JM->Width) / 2;
  lblChars->Left = JM->Left;
  if (lblChars->Left < 8)
    lblChars->Left = 8;
}

void __fastcall TfrmMain::DoJMValidateChar(TObject *Sender, WideChar AChar, bool &Valid)
{
  Valid |= chkDisplayAll->Checked;
}

void __fastcall TfrmMain::DisplayInfo(WideChar AChar)
{
  reInfo->Clear();
  reInfo->Lines->Add("Character Type: " + GetTypeString1(AChar));
  reInfo->Lines->Add("Bidirectional Layout: " + GetTypeString2(AChar));
  reInfo->Lines->Add("Text Processing:" + GetTypeString3(AChar));
  reInfo->Lines->Add(Format("Keyboard Code: U+%.4x, Alt+%.4d", ARRAYOFCONST((static_cast<int>(AChar), static_cast<int>(AChar)))));
  reInfo->Hint = Trim(reInfo->Lines->Text);
}

//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormCreate(TObject *Sender)
{
  JM = new TJvCharMap(this);
//  JM->Align = alClient;
  JM->Parent = this;
  JM->CharRange->EndChar = 255;
  JM->OnSelectChar = DoJMSelectChar;
//  JM->OnKeyUp = DoJMKeyUp;
//  JM->OnMouseUp = DoJMMouseUp;
//  JM->OnMouseWheel = DoJMMouseWheel;
  JM->OnResize = DoJMResize;
  JM->Anchors.Clear();
  JM->Anchors << akTop << akBottom;
  JM->PopupMenu = PopupMenu1;
  JM->OnValidateChar = DoJMValidateChar;
  JM->AutoSizeWidth = true;
  JM->Left = (ClientWidth - JM->Width) / 2;
  JM->Top = lblChars->Top + lblChars->Height + 2;
  JM->Height = Panel1->Top - JM->Top - 20;
  lblChars->FocusControl = JM;

  chkZoomPanel->Checked = JM->ShowZoomPanel;
  udStart->Position = JM->CharRange->StartChar;
  udEnd->Position = JM->CharRange->EndChar;
  udColumns->Position = JM->Columns;
  cbColor->ColorValue = JM->Color;
  cbFont->FontName = JM->Font->Name;
  cbColor->OnChange = cbColorChange;
  cbFont->OnChange = cbFontChange;
  chkShadow->Checked = JM->ShowShadow;

  edCharacter = new TEdit(this);
  edCharacter->Parent = Panel1;
  edCharacter->Left = reInfo->Left;
  edCharacter->Top = btnSelect->Top + 4;
  edCharacter->Width = btnSelect->Left - reInfo->Left - 7;
  edCharacter->Height = 22;
  edCharacter->Anchors.Clear();
  edCharacter->Anchors << akLeft << akTop << akRight;
  edCharacter->TabOrder = 11;
  FillFilter();
  FillLocales();
  ActiveControl = JM;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnFontClick(TObject *Sender)
{
  FontDialog1->Font = JM->Font;
  if (FontDialog1->Execute())
  {
    JM->Font = FontDialog1->Font;
    cbFont->FontName = JM->Font->Name;
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chkZoomPanelClick(TObject *Sender)
{
  JM->ShowZoomPanel = chkZoomPanel->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chkUnicodeClick(TObject *Sender)
{
  cbFilter->Enabled = chkUnicode->Checked;
  if (chkUnicode->Checked)
    JM->CharRange->Filter = static_cast<TJvCharMapUnicodeFilter>(cbFilter->ItemIndex);
  else
  {
    JM->CharRange->Filter = ufUndefined;
    JM->CharRange->EndChar = udEnd->Position;
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::Copy1Click(TObject *Sender)
{
  JvClipboard()->AsWideText = WideString(JM->Character);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnSelectClick(TObject *Sender)
{
  edCharacter->Text += JM->Character;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::cbFilterClick(TObject *Sender)
{
  if (chkUnicode->Checked && (cbFilter->ItemIndex > -1))
    JM->CharRange->Filter = static_cast<TJvCharMapUnicodeFilter>(cbFilter->ItemIndex);
  DisplayInfo(JM->Character);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::cbLocalesClick(TObject *Sender)
{
  if (cbLocales->ItemIndex > -1)
    JM->Locale = reinterpret_cast<LCID>(cbLocales->Items->Objects[cbLocales->ItemIndex]);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chkShadowClick(TObject *Sender)
{
  JM->ShowShadow = chkShadow->Checked;
  JM->ShadowSize = random(4) + 2;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chkDisplayAllClick(TObject *Sender)
{
  JM->Invalidate();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::edColsChange(TObject *Sender)
{
  if (!Changing)
  {
    Changing = true;
    try
    {
      JM->Columns = StrToInt(edCols->Text);
    }
    catch(...)
    {
      JM->Columns = 16;
      edCols->Text = "16";
      udColumns->Position = 16;
    }
    Changing = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::edEndChange(TObject *Sender)
{
  if (!Changing)
  {
    Changing = True;
    try
    {
      JM->CharRange->EndChar = StrToInt(edEnd->Text);
    }
    catch(...)
    {
      JM->CharRange->EndChar = 255;
      edEnd->Text = "255";
      udEnd->Position = 255;
    }
    Changing = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::edStartChange(TObject *Sender)
{
  if (!Changing)
  {
    Changing = true;
    try
    {
      JM->CharRange->StartChar = StrToInt(edStart->Text);
    }
    catch(...)
    {
      JM->CharRange->StartChar = 0;
      edStart->Text = "0";
      udStart->Position = 0;
    }
    Changing = false;
  }
}
//---------------------------------------------------------------------------
