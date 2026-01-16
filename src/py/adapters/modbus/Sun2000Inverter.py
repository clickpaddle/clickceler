#!/usr/bin/env python3.10
"""
Test example: simple Modbus TCP read from an MTU device
To be integrated later as an adatper to integrate with the CLickcelr Light MQTTS/KAFKA
instead of a SQL DB.

This script connects to a Huawei Sun2000 inverter via Modbus TCP,
samples input, active, and reactive power values over a period of time,
calculates average values, reads the daily energy yield, and stores
the data in a MySQL database for monitoring or logging purposes.

It runs in an infinite loop, taking readings every 5 seconds for
a total of RA (120) samples per cycle, then computes averages and logs them.
"""

import os
import time
import mysql.connector as db
from datetime import datetime
from sun2000_modbus import inverter, registers

# Configuration
os.environ['TZ'] = 'Europe/Brussels'
time.tzset()

RA = 120  # number of readings (5 sec * 120 = 10 min)
HOST_INVERTER = "10.0.1.32" # Replace with the IP address of the MTU
DB_CONFIG = {
    "host": "10.0.1.1" ,     # Replace with the IP address of your DB or eventual DB PRoxy
    "port": 4006,	    # Replace with the listening port of DB or eventual DB proxy
    "user": "energy",	     # Replace with your userid
    "password": "*********", # Replace with your password
    "database": "lora",      # Table space  in yout DB to store Data 
}

# Connect to inverter
inv = inverter.Sun2000(host=HOST_INVERTER)
inv.connect()

# Read serial number
SN = inv.read(registers.InverterEquipmentRegister.SN)

if inv.isConnected():
    while True:
        PP, AP, RP = [], [], []
        DE = None

        # Sampling loop
        for _ in range(RA):
            try:
                input_power = inv.read(registers.InverterEquipmentRegister.InputPower)
                PP.append(float(input_power))

                active_power = inv.read(registers.InverterEquipmentRegister.ActivePower)
                AP.append(float(active_power))

                reactive_power = inv.read(registers.InverterEquipmentRegister.ReactivePower)
                RP.append(float(reactive_power))

            except Exception:
                inv.disconnect()
                inv.connect()

            try:
                DE = inv.read(registers.InverterEquipmentRegister.DailyEnergyYield)
            except Exception:
                inv.disconnect()
                inv.connect()

            time.sleep(5)

        # Compute averages
        ip = round(sum(PP) / RA, 3) if PP else 0
        ap = round(sum(AP) / RA, 3) if AP else 0
        rp = round(sum(RP) / RA, 3) if RP else 0
        de = DE if DE is not None else 0

        # Debug output
        print(
            f"[{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}] "
            f"SN: {SN} | Input: {ip} W | Active: {ap} W | Reactive: {rp} VA | Daily Energy: {de} kWh"
        )

        # SQL insertion
        try:
            cnx = db.connect(**DB_CONFIG)
            cnx.autocommit = True
            cur = cnx.cursor()
            query = (
                "INSERT INTO sun2000 (time, sn, ip, DE, ap, rp) "
                f"VALUES (UTC_TIMESTAMP(), '{SN}', {ip}, {de}, {ap}, {rp});"
            )
            cur.execute(query)
        except db.Error as e:
            print(f"MySQL Error: {e}")

