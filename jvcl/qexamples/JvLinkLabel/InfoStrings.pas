{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: InfoStrings.pas, released 2002-01-05.

The Initial Developer of the Original Code is David Polberger <dpol@swipnet.se>
Portions created by David Polberger are Copyright (C) 2002 David Polberger.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: 2002-01-05;
Current Version: 1.00

You may retrieve the latest version of this file at the Project JEDI home page,
located at http://www.delphi-jedi.org

Known Issues:
  Please see the accompanying documentation.
-----------------------------------------------------------------------------}

unit InfoStrings;

interface

const
  NmrOfScreens = 8;
  Headings: array[0..NmrOfScreens - 1] of string =
    ('Introduction',
     'Formatting text',
     'Supported tags',
     'Handling links',
     'Dynamic tags',
     'Play!',
     'Source code',
     'Read more');

  Info: array[0..NmrOfScreens - 1] of string =
    (// Introduction
     'Welcome to <b>TJvLinkLabel!</b> Have you ever wanted to include ' +
     'formatted text and links that flow with the rest of the text in your ' +
     'applications? TJvLinkLabel is a fast, light-weight replacement for ' +
     'Delphi''s TLabel, that handles all this. In fact, the text you''re ' +
     'reading right now, is displayed by the TJvLinkLabel.' +
     '<p>' +
     'TJvLinkLabel is licensed under the <link>MPL</link> license, which ' +
     'basically means that if you alter the component and distribute an ' +
     'application incorporating it, you must make your changes to the ' +
     'component public. Note, though, that this doesn''t affect your ' +
     'application. Thus, you can safely use TJvLinkLabel in applications ' +
     'whose source code you want to remain private.' +
     '<p>' +
     'Have you seen the source code of a HTML document? This component uses ' +
     'the same technique to format text. <link>Read on</link> to find out ' +
     'more!',
     // Formatting text
     'We''ll start off with an example:' +
     '<p>' +
     'This text is <b>&lt;b&gt;</b>bold, and <b>&lt;i&gt;</b>italic' +
     '<b>&lt;/i&gt;</b>, and includes<b>&lt;/b&gt;</b> a <b>&lt;link&gt;</b>' +
     'link<b>&lt;/link&gt;</b>!' +
     '<p>' +
     'Simply enter this text in the Caption property of the TJvLinkLabel, ' +
     'just as you usually do with the TLabel component. The result looks ' +
     'something like this:' +
     '<p>' +
     'This text is <b>bold, and <i>italic</i>, and includes</b> a ' +
     '<link>link</link>!' +
     '<p>' +
     'For example, to start a section to be displayed as <b>bold text</b>, ' +
     'you''ll need to insert a "tag", in this case <b>&lt;b&gt;</b>. End the ' +
     'bold section with <b>&lt;/b&gt;</b>.' +
     '<p>' +
     'For a full listing of supported tags, <link>go on to the next ' +
     'page!</link>',
     // Supported tags
     '<b>Basic text formatting</b><br>' +
     '<b>&lt;b&gt;</b> - <b>Bold text</b><br>' +
     '<b>&lt;i&gt;</b> - <i>Italic text</i><br>' +
     '<b>&lt;u&gt;</b> - <u>Underlined text</u><br>' +
     '<b>&lt;color=[TCOLOR]&gt;</b> - <color=clBlue>Font Color</color>' +
     '<p>' +
     '<b>Line and paragraph breaks</b><br>' +
     '<b>&lt;br&gt;</b> - Line break (no end tag)<br>' +
     '<b>&lt;p&gt;</b> - Paragraph break (no end tag)' +
     '<p>' +
     '<b>Special features</b><br>' +
     '<b>&lt;link&gt;</b> - <link>Hypertext links</link><br>' +
     '<b>&lt;dynamic&gt;</b> - Text whose contents change ' +
     '<link>dynamically</link> (no end tag)',
     // Handling links
     'What happens when a user clicks a link? The <i>OnLinkClick</i> event ' +
     'handler fires, making it easy for you to perform any action you want. ' +
     'You may, for instance, open the user''s web browser, displaying a web ' +
     'page, or you can bring up a dialog window in your application, or ' +
     'something entirely different.' +
     '<p>' +
     'The <i>LinkNumber</i> parameter tells you which link the user ' +
     'clicked, while the <i>LinkText</i> tells you the contents of the ' +
     'link (with any tags removed).',
     // Dynamic tags
     '<i>Dynamic content</i> is used for content which may not be known when ' +
     'you design the form, or content that changes during the course of ' +
     'time, such as a label reporting memory usage. While you could simply ' +
     'assign a new string to the <i>Caption</i> property, you''d have to ' +
     'store the information both in the form file, and in the code itself ' +
     '(or only the latter). To add insult to injury, the entire string would ' +
     'have to be reparsed, negatively impacting performance. Also, this ' +
     'solution is awkward at best, and hard to maintain.' +
     '<p>' +
     'Enter dynamic tags. Specify which content changes dynamically by ' +
     'replacing the text itself with <b>&lt;dynamic&gt;</b>. You''ll need to ' +
     'handle the <i>OnDynamicTagInit</i> event, to give these tags default ' +
     'values. At run-time, simply call <i>FMyLinkLabel.UpdateDynamicTag</i> ' +
     'to update the contents of the dynamic tag at will!',
     // Play!
     '<link>Click here</link> to open a separate window, where you can test ' +
     'the TJvLinkLabel interactively! Feel free to edit the text in the edit ' +
     'box (or replace it completely), to see how TJvLinkLabel renders your ' +
     'input. On the right, you''ll see a tree representation of the contents ' +
     'of the edit box.' +
     '<p>' +
     '<b>Note:</b> I believe that the text in the separate window is used in ' +
     'various desktop publishing products as placeholder text - this copy ' +
     'was found on the Internet. As I don''t speak Latin, I have no idea ' +
     'what it means, if anything.', 
     // Source code
     'I believe that TJvLinkLabel''s source code is clearly written and easy ' +
     'to follow. Feel free to contribute!' +
     '<p>' +
     'TJvLinkLabel consists primarily of the component itself, a parser ' +
     'which tries to make sense of the contents of the <i>Caption</i> ' +
     'property and returns a tree representation of this data, and a ' +
     'renderer, which renders the output to your screen. Thanks to ' +
     'interfaces, you can easily replace either of these two components, ' +
     'provided that your class implements the same interfaces that the ' +
     'default parser and renderer are required to. The renderer should also ' +
     'be quite easy to extend, with your own tags.' +
     '<p>' +
     'Please read the <link>full documentation</link> for pointers on ' +
     'features I''d like to see implemented in the future, and any bugs ' +
     'which I would love to see squashed. You''ll also find comments ' +
     'pertaining to areas where performance could be improved, and areas of ' +
     'the code that could be more modular. In short, if you''d like to hack ' +
     'the code, see this document for useful tips!',
     // Read more
     'At this time, the documentation is maintained as a standard text file, ' +
     'although we plan to provide it either as XHTML/CSS or as a help file ' +
     'in the future. Look for it in the TJvLinkLabel directory!' +
     '<p>' +
     'Thanks for using TJvLinkLabel!' +
     '<p>' +
     '<i>David Polberger<br>' +
     'Author</i>');

  Lorem =
    '<b>Lorem ipsum dolor <i>sit</i> amet,</b> consectetuer adipiscing elit, ' +
    'sed diam nonummy nibh euismod <link>t<b>i<i>n</i>c</b>i<i>d</i>u<b>n</b>' +
    't <b>ut</b> laoreet</link> dolore magna aliquam erat volutpat. Ut wisi ' +
    'enim ad minim veniam,quis nostrud exerci tation ullamcorper suscipit ' +
    'lobortis nisl ut aliquip ex ea commodo consequat. Duis autem vel eum ' +
    '<b>iriure</b> dolor in hendrerit in vulputate velit esse molestie ' +
    'consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et ' +
    '<link>accumsan et iusto odio <i>dignissim qui <u>blandit praesent ' +
    'luptatum</u> zzril delenit augue duis dolore te feugait nulla facilisi. ' +
    'Lorem ipsum dolor</i> sit</link> amet, consectetuer adipiscing elit, ' +
    'sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam ' +
    'erat volutpat. <color=clTeal>Ut wisi enim ad <i>minim veniam, quis nostrud exerci ' +
    'tation ullamcorper suscipit lobortis nisl </i>ut aliquip ex ea commodo ' +
    'consequat.</color>' +
    '<p>' +
    'Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse ' +
    'molestie consequat, vel <i>illum</i> dolore eu feugiat nulla facilisis ' +
    'at vero eros et accumsan et iusto odio dignissim qui blandit praesent ' +
    'luptatum zzril delenit <b>augue duis</b> dolore te feugait nulla ' +
    'facilisi. Nam liber tempor cum soluta nobis eleifend option congue ' +
    'nihil imperdiet doming <link>id quod mazim</link> placerat facer possim ' +
    'assum.'{ +
    '<p>' +
    'Lorem ipsum <i>dolor sit</i> amet, consectetuer adipiscing elit, sed diam ' +
    'nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat ' +
    'volutpat. Ut wisi enim <b>ad</b> minim veniam, quis nostrud exerci tation ' +
    'ullamcorper suscipit lobortis nisl ut aliquip ex ea commodo consequat. ' +
    'Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse ' +
    'molestie consequat, vel <link>illum dolore <u>eu</u> feugiat nulla ' +
    'facilisis at vero eros et </link>accumsan et iusto odio dignissim qui ' +
    'blandit praesent luptatum zzril delenit augue duis <b>dolore te ' +
    'feugait</b> nulla facilisi. Lorem ipsum dolor sit amet, consectetuer ' +
    'adipiscing elit, sed diam nonummy nibh euismod tincidunt ut <link>' +
    'laoreet dolore</link> magna aliquam erat volutpat.' +
    '<p>' +
    '<b>Ut wisi enim</b> ad minim veniam, quis nostrud exerci <link>tation ' +
    'ullamcorper suscipit</link> lobortis nisl ut aliquip ex ea commodo ' +
    'consequat. Duis autem vel eum iriure dolor in hendrerit in vulputate ' +
    'velit esse molestie consequat, vel <b>illum <i>dolore eu <u>feugiat ' +
    'nulla </u>facilisis at vero eros et accumsan et iusto</i> odio ' +
    'dignissim</b> qui blandit praesent luptatum zzril delenit augue duis ' +
    'dolore te feugait nulla facilisi. Lorem ipsum dolor sit amet, ' +
    'consectetuer adipiscing elit, <link>sed diam <i>nonummy </i></link>' +
    'nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.' +
    '<p>' +
    'Ut wisi enim ad minim veniam, quis nostrud exerci tation ullamcorper ' +
    'suscipit lobortis nisl ut aliquip ex ea commodo consequat. Duis autem ' +
    'vel eum iriure <b>dolor in</b> hendrerit <link>in vulputate <i>velit' +
    '</i></link> esse molestie consequat, vel illum dolore eu feugiat nulla ' +
    'facilisis at vero eros et accumsan et iusto odio dignissim <b>qui ' +
    'blandit</b> praesent luptatum zzril delenit augue duis dolore te ' +
    'feugait nulla facilisi. ' +
    '<p>' +
    'Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam ' +
    'nonummy nibh euismod <u>tincidunt</u> ut laoreet dolore magna aliquam ' +
    'erat volutpat. Ut wisi enim ad minim veniam, quis nostrud exerci tation ' +
    'ullamcorper <link>suscipit lobortis</link> nisl ut aliquip ex ea ' +
    'commodo consequat. Duis autem vel eum iriure dolor in hendrerit in ' +
    'vulputate velit esse molestie consequat, vel illum <i>dolore eu <b>' +
    'feugiat</b></i> nulla facilisis at.' +
    '<p>' +
    'Vero eros et accumsan <link>et iusto</link> odio dignissim qui blandit ' +
    'praesent luptatum zzril delenit augue duis dolore te feugait nulla ' +
    'facilisi. Lorem ipsum dolor sit <b>amet</b>, consectetuer adipiscing ' +
    'elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna ' +
    'aliquam erat volutpat. Ut wisi enim ad <i>minim veniam</i>, quis ' +
    'nostrud exerci tation ullamcorper suscipit lobortis nisl ut aliquip ex ' +
    'ea commodo consequat.' +
    '<p>' +
    '<b>Autem vel</b> eum iriure dolor in hendrerit in vulputate velit esse ' +
    'molestie consequat, <link>vel illum</link> dolore eu feugiat nulla ' +
    'facilisis at vero eros et accumsan et <i>iusto odio dignissim</i> qui ' +
    'blandit praesent luptatum zzril delenit augue <b>duis dolore</b> te ' +
    'feugait nulla facilisi.'};

implementation

end.
