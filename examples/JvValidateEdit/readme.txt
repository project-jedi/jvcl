TJvValidateEdit

unit = JvValidateEdit

inherits from TJvCustomEdit

Introduced behaviour:
=====================

Properties:
===========

[Public] 

AsCurrency: Currency
The value as a Currency

AsFloat: Double
The value as a Double

AsInteger: Integer
The value as an integer

[Published]

AutoAlignment: boolean
When True, the Alignment is automatically assigned to taRightJustify for numbers and taLeftJustify for text when the DisplayFormat changes.

CheckChars: string 
The characters that are allowed to be entered (or in the case of dfNonCheckChars, *not* allowed)
True numeric types (dfCurrency, dfFloat, dfInteger, dfPercent, dfScientific and dfYear) are allowed to have a leading + or - as well.
Changing CheckChars triggers a re-Validation of the Text.

CriticalPoints: TCriticalPoints
Critical points only work for the numeric DisplayFormat styles. Unlike MaxValue and MinValue, you can exceed a CriticalPoint, but it will apply a colour to the value when it falls outside the range specified. CriticalPoints.MinValue and CriticalPoints.MaxValue define the range in which the Value has the original colour set for the Font. If the Value falls below CriticalPoints.MinValue, the Value gets the colour CriticalPoints.ColorBelow and if the Value goes above CriticalPoints.MaxValue, the Value gets the colour CriticalPoints.ColorAbove. CriticalPoints.CheckPoints defines how many points are checked, and may have the values cpNone, cpMax or cpBoth. cpNone is the default and means no colouring is done; cpMaxValue means that only the MaxValue is checked; cpBoth mean both the MaxValue and the MinValue are checked. 

DecimalPlaces: Cardinal 
For numeric types that support decimals (dfCurrency, dfFloat, dfPercent, and dfScientific), the number of decimals displayed. The actual edit value will be rounded to this number of decimal places if a greater precision is entered.
dfCurrency may only have up to 4 decimals.

DisplayFormat: enumerated type
dfAlphabetic, dfAlphaNumeric, dfBinary, dfCheckChars, dfCurrency, dfCustom, dfFloat, dfHex, dfInteger, dfNonCheckChars, dfNone, dfOctal, dfPercent, dfScientific, dfYear
Controls the keys that can be entered, and how the display is formatted.
Modifies CheckChars to the appropriate characters to check. 
Changing DisplayFormat triggers a re-Validation of the Text.

DisplayPrefix: string
A string that will be prepended to the displayed value. For example, to show Australian currency you would set the DisplayFormat to dfCurrency and the DisplayPrefix to 'AU' so a Value of 10 would display 'AU$10.00'

DisplaySuffix: string
A string that will be appended to the displayed value.

EditText: string
The text as entered (unformatted)

HasMaxValue: boolean
Specifies whether the property MaxValue is to be checked to ensure the value entered is within the range.

HasMinValue
Specifies whether the property MinValue is to be checked to ensure the value entered is within the range.

MaxValue: Double
The maximum value the entered value can have

MinValue: Double
The minimum value the entered value can have

Text: string
The formatted text

Value: Variant
The value of the entered value. The type will vary depending on DisplayFormat

ZeroEmpty: Boolean
Whether to display an empty edit box when the value is 0

Events:
=======

OnCustomValidate(Sender: TObject; Key: Char; const AText: string; var IsValid: boolean)
Called on each keystroke when the DisplayFormat is dfCustom so the user can validate the characters

OnValueChanged(Sender: TObject)
Triggers when the value is changed, either by an assignment of a value or on exit from the field if the value of the field as been edited

Problems:
=========

TJvValidateFormat uses the SysUtils.Format function to format numeric values. While this uses the Windows regional settings for the currency symbol, decimal separator and thousands separator, it does not format using the negative symbol, negative number format, negative currency format and positive currency format. This could be rectified by a custom written formatting routine. 

