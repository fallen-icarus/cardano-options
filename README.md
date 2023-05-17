# Cardano-Options

## Important Comments
There is no way to specify what datum should be for premium payments. **Do not use a plutus payment script address for your writer's address.**

## Fee Estimations

create assets UTxO = 0.628372 ADA
create one proposal UTxO = 0.626045 ADA
create two proposal UTxOs = 0.703411 ADA
create three proposal UTxOs = 0.780778 ADA
close an assets UTxO = 0.797346 ADA
close one proposal UTxO = 0.802720 ADA
close two proposal UTxOs = 0.935584 ADA
close three proposal UTxOs = 1.133239 ADA
purchase an option contract = 1.111633 ADA
close an expired contract UTxO = 0.826764 ADA
burn expired contractID = 0.553654 ADA
execute contract = 1.033613 ADA
update address = 0.575059 ADA

## Example Query Responses

Available Contracts Query
``` JSON
[
  {
    "address": "addr_test1zp9xg507keq0xueuxlsrz3tum0pjcayvuf5lg2064npmr7eualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aqvw5tff",
    "assets_utxo": {
      "info": {
        "beacon_symbol": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d",
        "current_asset": "lovelace",
        "desired_asset": "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
        "quantity": 10000000
      },
      "output_index": "0",
      "tx_hash": "07867c05d9d11c078b0193cd01f62a443962b474010f0e4b260840f6c832a921",
      "utxo_assets": [
        {
          "asset": "lovelace",
          "quantity": 15000000
        },
        {
          "asset": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d.417373657473",
          "quantity": 1
        }
      ]
    },
    "proposed_utxos": [
      {
        "info": {
          "beacon_symbol": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d",
          "current_asset": "lovelace",
          "desired_asset": "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
          "expiration_slot": 27959085,
          "premium": 2000000,
          "premium_asset": "lovelace",
          "quantity": 10000000,
          "strike_price": {
            "denominator": 1000000,
            "numerator": 1
          },
          "writer_address_payment_pubkey_hash": "fe90abc294e5f876d44f9b39583f2e6d905322c4735e3bda2928342f",
          "writer_address_payment_script_hash": null,
          "writer_address_staking_pubkey_hash": null,
          "writer_address_staking_script_hash": null
        },
        "output_index": "0",
        "tx_hash": "07e2cae3181f453804634460f0a1e95d864e17844bb4f67a64f8e9222b9e8190",
        "utxo_assets": [
          {
            "asset": "lovelace",
            "quantity": 3000000
          },
          {
            "asset": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d.50726f706f736564",
            "quantity": 1
          }
        ]
      },
      {
        "info": {
          "beacon_symbol": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d",
          "current_asset": "lovelace",
          "desired_asset": "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
          "expiration_slot": 27956085,
          "premium": 1500000,
          "premium_asset": "lovelace",
          "quantity": 10000000,
          "strike_price": {
            "denominator": 1000000,
            "numerator": 1
          },
          "writer_address_payment_pubkey_hash": "fe90abc294e5f876d44f9b39583f2e6d905322c4735e3bda2928342f",
          "writer_address_payment_script_hash": null,
          "writer_address_staking_pubkey_hash": null,
          "writer_address_staking_script_hash": null
        },
        "output_index": "1",
        "tx_hash": "07e2cae3181f453804634460f0a1e95d864e17844bb4f67a64f8e9222b9e8190",
        "utxo_assets": [
          {
            "asset": "lovelace",
            "quantity": 3000000
          },
          {
            "asset": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d.50726f706f736564",
            "quantity": 1
          }
        ]
      }
    ]
  }
]
```

Own Assets
``` JSON
[
  {
    "info": {
      "beacon_symbol": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d",
      "current_asset": "lovelace",
      "desired_asset": "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
      "quantity": 10000000
    },
    "output_index": "0",
    "tx_hash": "07867c05d9d11c078b0193cd01f62a443962b474010f0e4b260840f6c832a921",
    "utxo_assets": [
      {
        "asset": "lovelace",
        "quantity": 15000000
      },
      {
        "asset": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d.417373657473",
        "quantity": 1
      }
    ]
  }
]
```

Own Proposals
``` JSON
[
  {
    "info": {
      "beacon_symbol": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d",
      "current_asset": "lovelace",
      "desired_asset": "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
      "expiration_slot": 27959085,
      "premium": 2000000,
      "premium_asset": "lovelace",
      "quantity": 10000000,
      "strike_price": {
        "denominator": 1000000,
        "numerator": 1
      },
      "writer_address_payment_pubkey_hash": "fe90abc294e5f876d44f9b39583f2e6d905322c4735e3bda2928342f",
      "writer_address_payment_script_hash": null,
      "writer_address_staking_pubkey_hash": null,
      "writer_address_staking_script_hash": null
    },
    "output_index": "0",
    "tx_hash": "07e2cae3181f453804634460f0a1e95d864e17844bb4f67a64f8e9222b9e8190",
    "utxo_assets": [
      {
        "asset": "lovelace",
        "quantity": 3000000
      },
      {
        "asset": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d.50726f706f736564",
        "quantity": 1
      }
    ]
  },
  {
    "info": {
      "beacon_symbol": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d",
      "current_asset": "lovelace",
      "desired_asset": "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
      "expiration_slot": 27956085,
      "premium": 1500000,
      "premium_asset": "lovelace",
      "quantity": 10000000,
      "strike_price": {
        "denominator": 1000000,
        "numerator": 1
      },
      "writer_address_payment_pubkey_hash": "fe90abc294e5f876d44f9b39583f2e6d905322c4735e3bda2928342f",
      "writer_address_payment_script_hash": null,
      "writer_address_staking_pubkey_hash": null,
      "writer_address_staking_script_hash": null
    },
    "output_index": "1",
    "tx_hash": "07e2cae3181f453804634460f0a1e95d864e17844bb4f67a64f8e9222b9e8190",
    "utxo_assets": [
      {
        "asset": "lovelace",
        "quantity": 3000000
      },
      {
        "asset": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d.50726f706f736564",
        "quantity": 1
      }
    ]
  }
]
```

Specific Contract Info
``` JSON
[
  {
    "info": {
      "beacon_symbol": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d",
      "contract_id": "3c4d40a176b599ecb82d45a80795bb8219e1dcd4d84e0dde64f46ff872f7d52f",
      "current_asset": "lovelace",
      "desired_asset": "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
      "expiration_slot": 27974054,
      "premium": 2000000,
      "premium_asset": "lovelace",
      "quantity": 10000000,
      "strike_price": {
        "denominator": 1000000,
        "numerator": 1
      },
      "writer_address_payment_pubkey_hash": "fe90abc294e5f876d44f9b39583f2e6d905322c4735e3bda2928342f",
      "writer_address_payment_script_hash": null,
      "writer_address_staking_pubkey_hash": null,
      "writer_address_staking_script_hash": null
    },
    "output_index": "0",
    "tx_hash": "d6cc14ea86416fde3fba3626e75232c3bed0a445efd0ce8ebda99ac3cafa981d",
    "utxo_assets": [
      {
        "asset": "lovelace",
        "quantity": 15000000
      },
      {
        "asset": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d.3c4d40a176b599ecb82d45a80795bb8219e1dcd4d84e0dde64f46ff872f7d52f",
        "quantity": 1
      },
      {
        "asset": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d.416374697665",
        "quantity": 1
      }
    ]
  }
]
```

Own Active UTxOs
``` JSON
[
  {
    "info": {
      "beacon_symbol": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d",
      "contract_id": "82653d2221932d287f69f5169b7b96e87866cceae698e4a4e8f4ba1b2030caa6",
      "current_asset": "lovelace",
      "desired_asset": "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
      "expiration_slot": 27956685,
      "premium": 1500000,
      "premium_asset": "lovelace",
      "quantity": 10000000,
      "strike_price": {
        "denominator": 1000000,
        "numerator": 1
      },
      "writer_address_payment_pubkey_hash": "fe90abc294e5f876d44f9b39583f2e6d905322c4735e3bda2928342f",
      "writer_address_payment_script_hash": null,
      "writer_address_staking_pubkey_hash": null,
      "writer_address_staking_script_hash": null
    },
    "output_index": "0",
    "tx_hash": "4eb9dcf443c069103a8e5682daa070817936942110ed8fdf204b1ff1e8234bf0",
    "utxo_assets": [
      {
        "asset": "lovelace",
        "quantity": 15000000
      },
      {
        "asset": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d.416374697665",
        "quantity": 1
      },
      {
        "asset": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d.82653d2221932d287f69f5169b7b96e87866cceae698e4a4e8f4ba1b2030caa6",
        "quantity": 1
      }
    ]
  }
]
```

Own Contracts - looking from the perspective of the user with the contractID key
``` JSON
[
  {
    "asset": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d.82653d2221932d287f69f5169b7b96e87866cceae698e4a4e8f4ba1b2030caa6",
    "quantity": 1
  }
]
```