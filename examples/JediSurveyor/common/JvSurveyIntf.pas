{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

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

******************************************************************}

{$I jvcl.inc}

unit JvSurveyIntf;

interface

type
  {$IFNDEF COMPILER6_UP}
  {$M+}
  IInvokable = interface(IUnknown)
  end;
  {$M-}
  {$ENDIF COMPILER6_UP}

  TJvSurveyType = (
    stExclusive, // single choice
    stMultiple, // multiple choice
    stFreeForm); // type free-form text
  TJvSurveyFileFormat = (ffText, ffBinary);
  // one item in a survey
  IJvSurveyItem = interface(IInvokable)
    ['{D94E8215-2B22-44AE-9BE5-2B3784FAC7D9}']
    function GetID: integer;
    procedure SetID(const Value: integer);
    function GetTitle: WideString;
    procedure SetTitle(const Value: WideString);
    function GetDescription: WideString;
    procedure SetDescription(const Value: WideString);
    function GetSurveyType: TJvSurveyType;
    procedure SetSurveyType(const Value: TJvSurveyType);
    function GetChoices: WideString;
    procedure SetChoices(const Value: WideString);
    function GetResponses: WideString;
    procedure SetResponses(const Value: WideString);
    function GetComments: WideString;
    procedure SetComments(const Value: WideString);
    function GetRequired: WordBool;
    procedure SetRequired(const Value: WordBool);

    procedure SortResponses;

    property ID: integer read GetID write SetID;
    property Title: WideString read GetTitle write SetTitle;
    property Required: WordBool read GetRequired write SetRequired;
    property Description: WideString read GetDescription write SetDescription;
    property SurveyType: TJvSurveyType read GetSurveyType write SetSurveyType;
    property Choices: WideString read GetChoices write SetChoices;
    property Responses: WideString read GetResponses write SetResponses;
    property Comments: WideString read GetComments write SetComments;
  end;

  // a list of survey items
  IJvSurveyItems = interface(IInvokable)
    ['{B059D82D-0575-4E40-BAC2-38117CEAAC2C}']
    function GetItem(Index: integer): IJvSurveyItem;
    function GetCount: integer;

    function Add: IJvSurveyItem;
    procedure Delete(Index: integer);
    procedure Clear;
    procedure Sort;

    property Items[Index: integer]: IJvSurveyItem read GetItem; default;
    property Count: integer read GetCount;
  end;

  // info about person taking survey
  IJvSurveyTaker = interface(IInvokable)
    ['{FB6704CD-B5BE-4C43-9B91-429FA8B9968B}']
    function GetUserName: WideString;
    procedure SetUserName(const Value: WideString);
    function GetMailAddress: WideString;
    procedure SetMailAddress(const Value: WideString);
    function GetNotes: WideString;
    procedure SetNotes(const Value: WideString);
    function GetID: WideString;
    procedure SetID(const Value: WideString);
    property ID: WideString read GetID write SetID;
    property UserName: WideString read GetUserName write SetUserName;
    property MailAddress: WideString read GetMailAddress write SetMailAddress;
    property Notes: WideString read GetNotes write SetNotes;
  end;

  // the survey itself
  IJvSurvey = interface(IInvokable)
    ['{487E8927-AAC7-45BD-BCF3-26CDFB549676}']
    function GetID: integer;
    procedure SetID(const Value: integer);
    function GetTitle: WideString;
    procedure SetTitle(const Value: WideString);
    function GetDescription: WideString;
    procedure SetDescription(const Value: WideString);
    function GetItems: IJvSurveyItems;
    function GetRecipient: WideString;
    procedure SetRecipient(const Value: WideString);
    function GetRecipientMail: WideString;
    procedure SetRecipientMail(const Value: WideString);
    function GetReleaseDate: TDateTime;
    procedure SetReleaseDate(const Value: TDateTime);
    function GetExpiryDate: TDateTime;
    procedure SetExpiryDate(const Value: TDateTime);
    function GetResultHREF: WideString;
    procedure SetResultHREF(const Value: WideString);
    function GetSurveyTaker: IJvSurveyTaker;

    procedure LoadFromFile(const Filename: WideString);
    procedure SaveToFile(const Filename: WideString; Format: TJvSurveyFileFormat);

    property ID: integer read GetID write SetID;
    property Title: WideString read GetTitle write SetTitle;
    property Description: WideString read GetDescription write SetDescription;
    property Items: IJvSurveyItems read GetItems;
    property Recipient: WideString read GetRecipient write SetRecipient;
    property RecipientMail: WideString read GetRecipientMail write SetRecipientMail;
    property ResultHRef: WideString read GetResultHREF write SetResultHREF;
    property ReleaseDate: TDateTime read GetReleaseDate write SetReleaseDate;
    property ExpiryDate: TDateTime read GetExpiryDate write SetExpiryDate;
    property SurveyTaker: IJvSurveyTaker read GetSurveyTaker;
  end;

var
  CreateSurvey: function: IJvSurvey = nil;

implementation

end.

