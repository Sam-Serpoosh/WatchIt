# Watch It

![Watch It Piggy](http://confessionsofacouponclipper.com/wp-content/uploads/2012/01/Piggy-Bank-and-Coins.jpg)

## WHAT?

This is a very simple `Terminal App` written in **Haskell** which produces basic *stats* and *reports* off of the `Payment` data provided to it in **CSV** format. This is a tool I wrote for personal usage cause it's very handy to me and it has the EXACT use case that I care about, nothing more and nothing less :)

## DIFFERENT STATS & REPORT

It provides the following information based on given payment data:

- **Total** money paid.
- **Charts** what fraction of payments belong to each `Category`.
- **Warns** if more money was paid for a category regarding its defined *threshold* and shows hoe much over threshold it was paid.
- **Details** of each category is also rendered (e.g different payemnts in each category)

## SYNOPSIS

The following sections explain what an input should look like, where to put the input and how to configure `Categories` and their `Thresholds`. And finally how **build**, **test** and **run** this tool.

### REQUIREMENTS

You will need the followings in order to be able to use this tool:

- Haskell (Haskell2010 Lang)
- Cabal (a version which provides SandBox capabilities - >= 1.10)

### INPUT

create a directory named `payments` in the root directory of this repository and put your *payment-files* in there. For instance the following shows an excerpt of my `payments/july_costs` payment file:

```
11.00,transportation,uber to work
8.00,food,lunch
31.00,grocery,whole-food
9.00,fun,movie
10.00,food,dinner
```

### CONFIGURE CATEGORIES

You will probably have different payment categories than mine. So you need to change the `src/CategoryConfig.hs` file in order to specify categories and their respective *thresholds* for your case. For me it looks like the following (except that thresholds are NOT actually zero):

```haskell
module CategoryConfig where

import Category

-- Predefined categories and their threshold which can change
food, transportation, monthly, grocery, clothes, books, fun, others :: Category

food           = Cat { name = "food",            threshold = 0.00  }
transportation = Cat { name = "transportation",  threshold = 0.00  }
monthly        = Cat { name = "monthly",         threshold = 0.00  }
grocery        = Cat { name = "grocery",         threshold = 0.00  }
clothes        = Cat { name = "clothes",         threshold = 0.00  }
books          = Cat { name = "books",           threshold = 0.00  }
fun            = Cat { name = "fun",             threshold = 0.00  }
others         = Cat { name = "others",          threshold = 0.00  }

validCategories :: [Category]
validCategories = [
                    food,
                    transportation,
                    monthly,
                    grocery,
                    clothes,
                    books,
                    fun,
                    others
                  ]

```

### BUILD & RUN

When you're done putting your **input payment file** in `payments` directory and configuring your **categories** and their **thresholds** as explained above; you can run the program by issueing the following command from the root directory of the repository:

```shell
./generate_report.sh payments/your-payment-file
```

This will build the and create the `executable` file in the `dist` directory and then run the program.

### OUTPUT

The following shows the output for the payments in the sample input mentioned above:

```
* TOTAL PAID *
69.0
-------------------------------------------
-------------------------------------------
-------------------------------------------

* PAYMENTS CHART *
food          : ###
fun           : ##
grocery       : #####
transportation: ##

-------------------------------------------
-------------------------------------------
-------------------------------------------

!!! WARNINGS !!!

-------------------------------------------
-------------------------------------------
-------------------------------------------

* CATEGORY DETAILS *
food:  18.0
lunch  -> 8.0
dinner -> 10.0
-------------------------------------------
transportation:  11.0
uber to work -> 11.0
-------------------------------------------
grocery:  31.0
whole-food -> 31.0
-------------------------------------------
fun:  9.0
movie -> 9.0
-------------------------------------------
```

In this case payments of **NO** category was passed its respected threshold, hence, you see no `WARNINGS` in the output.

### RUNNING TESTS

If you wanna run the tests/specs which are written using `HSpec` for this application, you can simply run the following from the root directory of the repository:

```shell
cabal test
```

#### KEEP IN MIND

That there are couple of tests related to `warnings` function in `src/PaymentTrackerSpec.hs` file and those depend on the thresholds determined in `src/CategoryConfig.hs`. They are passing for me cause I wrote those tests regarding my thresholds and categories but if you configure your own categories and their thresholds, they might fail and you need to update those tests based on your own categories and thresholds. [of course there is an easy way to make that test independent of the config file but I'm too lazy at the time to do that!]