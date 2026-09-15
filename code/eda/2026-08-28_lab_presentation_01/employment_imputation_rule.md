# Employment Imputation Rule


## Variables

`E`, `U`, `D` and `O`: Possible employment states; employed, unemployed, discouraged, and out of labour force (not willing) respectively.

`curr_month_emp`: Employment status in current month (To be imputed).

`curr_wave_emp`: Employment status reported in current wave.

`prev_wave_emp`: Employment status reported in previous wave.

`wage_income`: Income from wages in current month.

## Imputation

```
if prev_wave_emp == NA:
    curr_month_emp = NA # Data from two consecutive waves not available
else:    
    if wage_income > 0:
        curr_month_emp = E
    else:
        # Zero wage income
        if prev_wave_emp != E:
            curr_month_emp = prev_wave_emp  # U/D/O from previous wave
        else:
            if curr_wave_emp != E:
                curr_month_emp = curr_wave_emp # U/D/O from current wave
            else:
                # Employed in both previous and current wave
                curr_month_emp = U
```