import hashlib
import itertools
import sys
import string

from operator import itemgetter

NUM_NODES = 3
NUM_PARTITIONS = 5


def hash_name(name):
    encoded_name = name.encode("utf-8")
    hash_encoded_name = hashlib.sha1(encoded_name).hexdigest()

    return int(hash_encoded_name[:7], 16)


def create_partitions(node_name, partitions):
    partition_hashes = []

    for partition_number in range(partitions):
        partition_name = f"{node_name}-{partition_number}"
        partition_hash = hash_name(partition_name)

        partition_hashes.append(
            {
                "min_hash": partition_hash,
                "partition_name": partition_name,
                "node_name": node_name,
            }
        )

    return partition_hashes


def create_routing_table(node_names, partitions):
    table = []

    for node_name in node_names:
        table.extend(create_partitions(node_name, partitions))

    table = sorted(table, key=itemgetter("min_hash"))

    return table


if NUM_NODES > len(string.ascii_lowercase):
    print("Too many servers")
    sys.exit(1)

nodes = [f"server-{i}" for i in string.ascii_lowercase[:NUM_NODES]]

routing_table = create_routing_table(nodes, NUM_PARTITIONS)
routing_table = [
    {
        "min_hash": 0,
        "partition_name": routing_table[-1]["partition_name"],
        "node_name": routing_table[-1]["node_name"],
    }
] + routing_table

routing_table_shift = routing_table[1:] + [
    {"min_hash": 0xFFFFFFF, "partition_name": "END"}
]

full_routing_table = []
for i, j in zip(routing_table, routing_table_shift):
    full_routing_table.append(
        {
            "min_hash": i["min_hash"],
            "partition_name": i["partition_name"],
            "node_name": i["node_name"],
            "served_hashes": j["min_hash"] - i["min_hash"],
        }
    )

print("Full routing table")
for r in full_routing_table:
    print(f'{r["min_hash"]:9} --> {r["partition_name"]} ({r["served_hashes"]} hashes)')

grouped_routing_table = itertools.groupby(
    full_routing_table, key=itemgetter("node_name")
)


simplified_routing_table = []
for r in grouped_routing_table:
    consecutive_partitions = list(r[1])

    simplified_routing_table.append(
        {
            "node_name": r[0],
            "min_hash": consecutive_partitions[0]["min_hash"],
            "served_hashes": sum([i["served_hashes"] for i in consecutive_partitions]),
        }
    )

print()
print("Simplified routing table")
for r in simplified_routing_table:
    print(f'{r["min_hash"]:9} -- > {r["node_name"]} ({r["served_hashes"]:8} hashes)')

print()
print("Stats")
stats = []
for node in nodes:
    slots = filter(lambda x: x["node_name"] == node, simplified_routing_table)
    total_hashes = sum([i["served_hashes"] for i in slots])
    stats.append({"node_name": node, "served_hashes": total_hashes})

for r in stats:
    print(r["node_name"], r["served_hashes"])

total_hashes = sum([i["served_hashes"] for i in stats])
print()
print(f"TOTAL HASHES: {total_hashes}/{2**28 - 1}")
